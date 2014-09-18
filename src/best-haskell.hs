{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import System.FilePath

import Web.Apiary
import Web.Apiary.Heroku
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Autohead
import Network.Wai.Handler.Warp
import qualified Web.Apiary.MongoDB   as M
import qualified Web.Apiary.Memcached as C
import qualified Database.Memcached.Binary.Maybe as C

import Control.Monad
import Control.Monad.Trans.Control
import Control.Applicative
import Control.Concurrent

import qualified Data.Binary as B
import qualified Data.Binary.Put as B
import qualified Data.Aeson as A
import Data.Abeson
import Data.Apiary.Extension
import Data.Word
import Data.Char
import Data.Typeable
import Data.Time
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as L

-- | global application config
data AppConfig = AppConfig { flushHandlerEnable :: Bool
                           , cacheTime          :: Int
                           , startEndCache      :: MVar (UTCTime, UTCTime)
                           }

-- | set configuration and run server with extensions.
serv :: (Extensions '[AppConfig, C.Memcached, M.MongoDB, Heroku] -> IO Application) -> IO ()
serv app = do
    let mongoConf = def { M.mongoDBTimeout    = 30
                        , M.mongoDBAccessMode = M.slaveOk
                        , M.numConnection     = 10
                        }
        mcConf    = def { C.cacheConfig = Just def
                        , C.connectInfo = def { C.numConnection = 10 }
                        }
    ac <- AppConfig False (600 * 10^(6::Int)) <$> newEmptyMVar
    herokuWith (M.initHerokuMongoDB mongoConf +> C.initHerokuMemcached mcConf +> initializer' (return ac))
        run def { herokuAppName = Just "best-haskell" } app

-- | cabal type (Executable|Library) for query.
data CabalType = Executable | Library deriving (Typeable, Show)
instance Path CabalType where
    readPath s = case T.toLower s of
        sl | sl == "library"    -> Just Library
           | sl == "executable" -> Just Executable
           | otherwise -> Nothing
    pathRep = typeRep

instance Query CabalType where
    readQuery (Just s) = case S8.map toLower s of
        sl | sl `elem` ["l", "lib", "library"]    -> Just Library
           | sl `elem` ["e", "exe", "executable"] -> Just Executable
           | otherwise -> Nothing
    readQuery Nothing = Nothing
    qTypeRep = typeRep

main :: IO ()
main = serv $ runApiary def $ do
    -- install middlewares
    middleware $ gzip def
    middleware autohead

    -- API

    -- global options
    ("limit" ?? "number to fetch ranking."                     =?!: (10 :: Word))
        . ("range" ?? "days of aggregation."                   =?:  pWord)
        . ("since" ?? "date of aggregation. instead of range." =?:  (Proxy :: Proxy Day))
        . switchQuery ("new" ?? "new package only") $ do

        -- /ranking most generic ranking api 
        [capture|/ranking|]
            . ("category" ?? "category filter."       =*: pText)
            . ("q"        ?? "package query string"   =?: pText)
            . ("type"     ?? "type of package(executable or library)" =?: (Proxy :: Proxy CabalType)) $ do

            accept "application/json" . method GET 
                . document "get package download ranking."
                . action $ \limit mbrange mbsince new cats q typ -> do
                    (st, ed) <- getDataStartEnd
                    rankingAction st ed limit q (mkSince (utctDay ed) mbrange mbsince) new cats typ >>= lazyBytes

    [capture|/categories|] . accept "application/json" . method GET
        . document "list of categories." . action $ categoriesAction

    [capture|/package/:T.Text[package name]|] . accept "application/json" . method GET
        . document "package information" . action $ \pkg -> do
            (_,ed) <- getDataStartEnd
            let key = L.toStrict . B.runPut $ do
                    B.putByteString "p"
                    B.putByteString (T.encodeUtf8 pkg)
                    B.put (toModifiedJulianDay $ utctDay ed)
            C.cacheMaybe key (M.access $ M.findOne (M.select ["name" M.=: pkg] "packages") {
                M.project = ["_id" M.=: (0::Int), "recent" M.=: (0::Int)]
                } >>= return . fmap (A.encode . toAeson def)) >>= \case
                Nothing -> status status404 >> bytes "package not found." >> stop
                Just p  -> lazyBytes p

    -- /flush flushAll memcached
    ac <- apiaryExt (Proxy :: Proxy AppConfig)
    when (flushHandlerEnable ac) $ [capture|/flush|] . method GET . action $ do
        _ <- C.memcached C.flushAll
        bytes "flush"

    -- / all information used in root page.
    [capture|/|] . accept "application/json" . method GET . ("category" =*: pText) . action $ rankingApiAction

    -- static files
    root           . method GET . action $ file "static/main.html"          Nothing
    [capture|/**|] . method GET . action $ \f -> file (joinPath $ "static" : map T.unpack f) Nothing

    -- other
    [capture|/nop|] . method GET . document "no operation to keep wake up on heroku." . action $ bytes "nop\n"
    [capture|/api/documentation|] . method GET . action $
        defaultDocumentationAction def { documentGoogleAnalytics = Just "UA-48784415-4" }

mkSince :: Day -> Maybe Word -> Maybe Day -> Maybe Day
mkSince end mbrange mbsince = case mbsince of
    Just s  -> Just s
    Nothing -> case mbrange of
        Nothing -> Nothing
        Just r  -> Just $ addDays (negate $ fromIntegral r) end

infixr 5 $.
($.) :: (a -> b) -> a -> b
($.) = ($)

getDataStartEnd :: (MonadIO m, MonadBaseControl IO m, Has M.MongoDB exts, Has AppConfig exts)
                => ActionT exts m (UTCTime, UTCTime)
getDataStartEnd = do
    mvar <- startEndCache <$> getExt Proxy
    liftIO (tryReadMVar mvar) >>= \case
        Nothing -> do
            (Just st', Just ed') <- M.access $ (,)
                <$> M.findOne (M.select ["key" M.=: ("recent_start" :: T.Text)] "config")
                <*> M.findOne (M.select ["key" M.=: ("last_update"  :: T.Text)] "config")
            let v = (M.at "value" st', M.at "value" ed')
            void . liftIO $ tryPutMVar mvar v
            delay <- cacheTime <$> getExt Proxy
            void . liftIO . forkIO . void $ threadDelay delay >> tryTakeMVar mvar
            return v
        Just c -> return c

memcacheRankingKey :: UTCTime -> UTCTime -> Word -> Maybe T.Text
                   -> Maybe Day -> Bool -> [T.Text] -> Maybe CabalType -> S8.ByteString
memcacheRankingKey (UTCTime st _) (UTCTime ed _) lim pkg mbsince new cat typ = L.toStrict . B.runPut $ do
    B.putByteString "r"
    B.put (toModifiedJulianDay st)
    B.put (toModifiedJulianDay ed)
    B.put lim
    B.put (fmap T.encodeUtf8 pkg)
    B.put (maybe 0 toModifiedJulianDay mbsince)
    B.put new
    mapM_ (B.put . T.encodeUtf8) cat
    B.put $ case typ of { Nothing -> 0; Just Library -> 1; Just Executable -> (2::Word8) }

memcacheCategoriesKey :: UTCTime -> UTCTime -> S8.ByteString
memcacheCategoriesKey (UTCTime st _) (UTCTime ed _) = L.toStrict . B.runPut $ do
    B.putByteString "c"
    B.put (toModifiedJulianDay st)
    B.put (toModifiedJulianDay ed)

rankingAction :: (MonadIO m, MonadBaseControl IO m, Has M.MongoDB exts, Has C.Memcached exts, Has AppConfig exts)
              => UTCTime -> UTCTime -> Word -> Maybe T.Text -> Maybe Day
              -> Bool -> [T.Text] -> Maybe CabalType -> ActionT exts m L.ByteString
rankingAction st ed limit pkg mbsince new cats typ = do
    let key = memcacheRankingKey st ed limit pkg mbsince new cats typ
    C.cache key $ do
        doc <- M.access $ rankingQuery (utctDay ed) limit pkg mbsince new cats typ
        return . A.encode $ A.object [ "ranking" A..= map (toAeson def . filter (\(l M.:= _) -> l /= "_id")) doc
                                     , "start"   A..= fmap (max st . flip UTCTime 0) mbsince
                                     , "end"     A..= ed
                                     ]

rankingApiAction :: (MonadIO m, MonadBaseControl IO m, Has M.MongoDB exts, Has C.Memcached exts, Has AppConfig exts)
                 => [T.Text] -> ActionT exts m ()
rankingApiAction cats = do
    (st, ed) <- getDataStartEnd
    let key = L.toStrict . B.runPut $
              B.putByteString "t" >> mapM_ (B.put . T.encodeUtf8) cats >> B.put (toModifiedJulianDay $ utctDay ed)
    r <- C.cache key $ do
        tot <- rankingAction st ed 10 Nothing Nothing                             False cats Nothing
        l1w <- rankingAction st ed 10 Nothing (Just $ addDays (-7)  $ utctDay ed) False cats Nothing
        new <- rankingAction st ed 10 Nothing Nothing                             True  cats Nothing
        n   <- M.access (M.count $ M.select (if null cats then [] else ["category" M.=: ["$in" M.=: cats]]) "packages")
        return . A.encode $ A.object
                [ "total"      A..= (A.decode tot :: Maybe A.Value)
                , "weekly"     A..= (A.decode l1w :: Maybe A.Value)
                , "new"        A..= (A.decode new :: Maybe A.Value)
                , "nPackages"  A..= n
                , "lastUpdate" A..= ed
                ]
    lazyBytes r

categoriesAction :: (MonadIO m, MonadBaseControl IO m, Has C.Memcached exts, Has M.MongoDB exts, Has AppConfig exts)
                 => ActionT exts m ()
categoriesAction = do
    (st, ed) <- getDataStartEnd
    let key = memcacheCategoriesKey st ed
    r <- C.cache key $ do
            doc <- M.access $ M.aggregate "packages"
                [ ["$project" M.=: ["category" M.=: (1 :: Int)]]
                , ["$unwind"  M.=: ("$category" :: T.Text)]
                , ["$group"   M.=: ["_id" M.=: ("$category" :: T.Text), "count" M.=: ["$sum" M.=: (1::Int)]]]
                , ["$sort"    M.=: ["_id" M.=: (1::Int), "count" M.=: (1::Int)]]
                ]
            return . A.encode $ A.object [ "categories" A..= map conv doc
                                         , "latest"     A..= ed
                                         ]
    lazyBytes r
  where
    conv b = A.object [ "category" A..= (M.at "_id"   b :: T.Text)
                      , "count"    A..= (M.at "count" b :: Int)
                      ]

rankingQuery ::  MonadIO m => Day -> Word -> Maybe T.Text
             -> Maybe Day -> Bool -> [T.Text] -> Maybe CabalType -> M.Action m [M.Document]
rankingQuery ed limit pkg mbsince new cats typ = M.aggregate "packages" $ case mbsince of
    Just since ->
        (if null filt then id else (["$match" M.=: filt]:)) $.
        ["$project" M.=: [ "recent"    M.=: M.Int64 1
                         , "synopsis"  M.=: M.Int64 1
                         , "author"    M.=: M.Int64 1
                         , "name"      M.=: M.Int64 1
                         , "category"  M.=: M.Int64 1
                         ]] :
        [ "$unwind" M.=: ("$recent" :: T.Text)] :
        [ "$match"  M.=: ["recent.date" M.=: ["$gt" M.=: M.UTC (UTCTime since 0)]]] :
        ["$group"   M.=: [ "_id"      M.=: ("$name" :: T.Text)
                         , "name"     M.=: ["$first" M.=: ("$name"         :: T.Text)]
                         , "synopsis" M.=: ["$first" M.=: ("$synopsis"     :: T.Text)]
                         , "author"   M.=: ["$first" M.=: ("$author"       :: T.Text)]
                         , "category" M.=: ["$first" M.=: ("$category"     :: T.Text)]
                         , "total"    M.=: ["$sum"   M.=: ("$recent.count" :: T.Text)]
                         ]] :
        ["$sort"    M.=: ["total" M.=: (-1 :: Int)]] :
        ["$limit"   M.=: M.Int64 (fromIntegral $ min 100 limit)] : []
    Nothing ->
        (if null filt then id else (["$match" M.=: filt]:)) $.
        ["$project" M.=: [ "total"    M.=: M.Int64 1
                         , "synopsis" M.=: M.Int64 1
                         , "author"   M.=: M.Int64 1
                         , "category" M.=: M.Int64 1
                         , "name"     M.=: M.Int64 1
                         ]] :
        ["$sort"    M.=: ["total" M.=: (-1 :: Int)]] :
        ["$limit"   M.=: M.Int64 (fromIntegral limit)] : []
        
  where
    filt = (if null cats then id else (("category"       M.=: ["$in" M.=: cats]):)) $
           (case pkg of {Nothing -> id; Just p -> (("name" M.=: M.Regex p "i"):)}) $
           (if not  new  then id else (("initialRelease" M.=: ["$gt" M.=: UTCTime (addDays (-31) ed) 0]):)) $
        case typ of
            Nothing         -> []
            Just Library    -> ["hasLibrary"  M.=: True]
            Just Executable -> ["executables" M.=: ["$ne" M.=: ([] :: [T.Text])]]