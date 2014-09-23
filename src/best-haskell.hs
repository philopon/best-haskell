{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
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
import Control.Concurrent.Lifted

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
                           , startEndCache      :: MVar (Day, Day)
                           }

initApp :: MonadIO m => Initializer' m AppConfig
initApp = initializer' . liftIO $
    AppConfig True  (600 * 10^(6::Int)) <$> newEmptyMVar

mongoConfig :: M.MongoDBConfig
mongoConfig = def { M.mongoDBTimeout    = 30
                  , M.mongoDBAccessMode = M.slaveOk
                  , M.numConnection     = 10
                  }

memcachedConfig :: C.MemcachedConfig
memcachedConfig = def { C.cacheConfig = Just def
                      , C.connectInfo = def { C.numConnection = 10 }
                      }

-- | combined extension initializer.
extensions :: Initializer IO '[Heroku] '[AppConfig, C.Memcached, M.MongoDB, Heroku]
extensions = M.initHerokuMongoDB mongoConfig +> C.initHerokuMemcached memcachedConfig +> initApp

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

instance B.Binary CabalType where
    get = undefined
    put Executable = B.put (0 :: Word8)
    put Library    = B.put (1 :: Word8)

-- cache key prefixes
data CacheCategory 
    = Ranking
    | Categories
    | Package
    | Top
    | Count
    | Licenses
    | Maintainers

putCacheCategory :: CacheCategory -> B.Put
putCacheCategory Ranking     = B.putByteString "r"
putCacheCategory Categories  = B.putByteString "c"
putCacheCategory Package     = B.putByteString "p"
putCacheCategory Top         = B.putByteString "t"
putCacheCategory Count       = B.putByteString "n"
putCacheCategory Licenses    = B.putByteString "l"
putCacheCategory Maintainers = B.putByteString "m"

main :: IO ()
main = herokuWith extensions run def {herokuAppName = Just "best-haskell"} $ runApiary def $ do
    -- install middlewares
    middleware $ gzip def
    middleware autohead

    -- API

    -- global options
    ("category" ?? "category filter."                       =*: pText)
        . ("maintainer" ?? "maintainer filter"                      =*: pText)
        . ("license"    ?? "license filter"                         =?: pText)
        . ("q"          ?? "package query string"                   =*: pText)
        . ("type"       ?? "type of package(executable or library)" =?: (Proxy :: Proxy CabalType))
        . switchQuery ("active" ?? "active package only")
        . switchQuery ("new"    ?? "new package only") $ do

        [capture|/ranking|]
            -- query parameters
            . ("range"    ?? "days of aggregation."                   =?: pWord)
            . ("since"    ?? "date of aggregation. instead of range." =?: (Proxy :: Proxy Day))
            . ("limit"    ?? "number to fetch ranking."               =?!: (10 :: Word))
            . ("skip"     ?? "skip data"                              =?: pWord)

            -- filters
            . accept "application/json"
            . method GET 
            . document "get package download ranking."
            . action $ \cats ms lcs pkg typ actv new mbrange mbsince limit mbskp -> do
                (st, ed) <- getDataStartEnd
                rankingAction st RankingQuery
                    { rankingEnd         = ed
                    , rankingLimit       = limit
                    , rankingSince       = mkSince ed mbrange mbsince
                    , rankingSkip        = mbskp
                    , rankingOnlyNew     = new
                    , rankingOnlyActive  = actv
                    , rankingCategories  = cats
                    , rankingType        = typ
                    , rankingPackage     = pkg
                    , rankingMaintainers = ms
                    , rankingLicense     = lcs
                    } >>= lazyBytes

        [capture|/count|] . accept "application/json" . method GET
            . document "count filtered packages."
            . action $ \cats ms lcs pkg typ actv new -> do
                (st, ed) <- getDataStartEnd
                let key = L.toStrict . B.runPut $ do
                        putCacheCategory Count
                        B.put (toModifiedJulianDay st)
                        B.put (toModifiedJulianDay ed)
                        B.put (fmap T.encodeUtf8 cats)
                        B.put (fmap T.encodeUtf8 ms)
                        B.put (fmap T.encodeUtf8 lcs)
                        B.put (fmap T.encodeUtf8 pkg)
                        B.put typ
                        B.put actv
                        B.put new
                lazyBytes =<< C.cache key
                    (fmap A.encode . M.access . M.count $ M.select (rankingFilter ed cats ms lcs pkg actv new typ) "packages")

    [capture|/categories|] . accept "application/json" . method GET
        . document "list of categories." . action $ categoriesAction

    [capture|/maintainers|] . accept "application/json" . method GET
        . document "list of maintainer." . action $ maintainersAction

    [capture|/licenses|] . accept "application/json" . method GET
        . document "list of license." . action $ licensesAction

    [capture|/package/:T.Text[package name]|]
        . accept "application/json" . method GET
        . document "package information" . action $ \pkg -> do
            (_,ed) <- getDataStartEnd
            let key = L.toStrict . B.runPut $ do
                    putCacheCategory Package
                    B.putByteString (T.encodeUtf8 pkg)
                    B.put           (toModifiedJulianDay ed)
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
    [capture|/tables|] . accept "application/json" . method GET
        . ("category"   =*: pText)
        . ("maintainer" =*: pText)
        . ("license"    =?: pText)
        . action $ rankingTablesAction

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
                => ActionT exts m (Day, Day)
getDataStartEnd = do
    mvar <- startEndCache <$> getExt Proxy
    liftIO (tryReadMVar mvar) >>= \case
        Nothing -> do
            (Just st', Just ed') <- M.access $ (,)
                <$> M.findOne (M.select ["key" M.=: ("recent_start" :: T.Text)] "config")
                <*> M.findOne (M.select ["key" M.=: ("last_update"  :: T.Text)] "config")
            let v = (utctDay $ M.at "value" st', utctDay $ M.at "value" ed')
            void . liftIO $ tryPutMVar mvar v
            delay <- cacheTime <$> getExt Proxy
            void . liftIO . fork . void $ threadDelay delay >> tryTakeMVar mvar
            return v
        Just c -> return c

rankingAction :: (MonadBaseControl IO m, Has C.Memcached exts, Has M.MongoDB exts, MonadIO m)
              => Day -> RankingQuery -> ActionT exts m L.ByteString
rankingAction st q = do
    C.cache key $ do
        doc <- M.access $ rankingQuery q
        return . A.encode $ A.object [ "ranking" A..= map (toAeson def . filter (\(l M.:= _) -> l /= "_id")) doc
                                     , "start"   A..= fmap (max (UTCTime st 0) . flip UTCTime 0) (rankingSince q)
                                     , "end"     A..= UTCTime (rankingEnd q) 0
                                     ]
  where
    key = L.toStrict . B.runPut $
        putCacheCategory Ranking >> B.put (toModifiedJulianDay st) >> B.put q

categoriesAction :: (MonadIO m, MonadBaseControl IO m, Has C.Memcached exts, Has M.MongoDB exts, Has AppConfig exts)
                 => ActionT exts m ()
categoriesAction = do
    (_, ed) <- getDataStartEnd
    (>>= lazyBytes) . C.cache (key ed) $ do
            doc <- M.access $ M.aggregate "packages"
                [ ["$project" M.=: ["category" M.=: (1 :: Int)]]
                , ["$unwind"  M.=: ("$category" :: T.Text)]
                , ["$group"   M.=: ["_id" M.=: ("$category" :: T.Text), "count" M.=: ["$sum" M.=: (1::Int)]]]
                , ["$sort"    M.=: ["_id" M.=: (1::Int), "count" M.=: (-1::Int)]]
                ]
            return . A.encode $ A.object [ "categories" A..= map conv doc
                                         , "latest"     A..= UTCTime ed 0
                                         ]
  where
    conv b = A.object [ "category" A..= (M.at "_id"   b :: T.Text)
                      , "count"    A..= (M.at "count" b :: Int)
                      ]
    key ed = L.toStrict . B.runPut $ do
        putCacheCategory Categories
        B.put (toModifiedJulianDay ed)

maintainersAction :: (MonadIO m, MonadBaseControl IO m, Has C.Memcached exts, Has M.MongoDB exts, Has AppConfig exts)
                  => ActionT exts m ()
maintainersAction = do
    (_, ed) <- getDataStartEnd
    (>>= lazyBytes) . C.cache (key ed) $ do
        doc <- M.access $ M.aggregate "packages"
            [ ["$project" M.=: ["maintainers" M.=: (1 :: Int)]]
            , ["$unwind"  M.=: ("$maintainers" :: T.Text) ]
            , ["$group"   M.=: ["_id" M.=: ("$maintainers" :: T.Text), "count" M.=: ["$sum" M.=: (1::Int)]]]
            , ["$sort"    M.=: ["_id" M.=: (1::Int), "count" M.=: (-1::Int)]]
            ]
        return . A.encode $ A.object [ "maintainers" A..= map conv doc
                                     , "latest"      A..= UTCTime ed 0
                                     ]
  where
    conv b = A.object [ "maintainer" A..= (M.at "_id"   b :: T.Text)
                      , "count"      A..= (M.at "count" b :: Int)
                      ]
    key ed = L.toStrict . B.runPut $ do
        putCacheCategory Maintainers
        B.put (toModifiedJulianDay ed)

licensesAction :: (MonadIO m, MonadBaseControl IO m, Has C.Memcached exts, Has M.MongoDB exts, Has AppConfig exts)
               => ActionT exts m ()
licensesAction = do
    (_, ed) <- getDataStartEnd
    (>>= lazyBytes) . C.cache (key ed) $ do
        doc <- M.access $ M.aggregate "packages"
            [ ["$project" M.=: ["license" M.=: (1 :: Int)]]
            , ["$group"   M.=: ["_id" M.=: ("$license" :: T.Text), "count" M.=: ["$sum" M.=: (1::Int)]]]
            , ["$sort"    M.=: ["count" M.=: (-1::Int), "_id" M.=: (1::Int)]]
            ]
        return . A.encode $ A.object [ "licenses" A..= map conv doc
                                     , "latest"   A..= UTCTime ed 0
                                     ]
  where
    conv b = A.object [ "license" A..= (M.at "_id"   b :: T.Text)
                      , "count"   A..= (M.at "count" b :: Int)
                      ]
    key ed = L.toStrict . B.runPut $ do
        putCacheCategory Licenses
        B.put (toModifiedJulianDay ed)

rankingTablesAction :: (MonadIO m, MonadBaseControl IO m, Has M.MongoDB exts, Has C.Memcached exts, Has AppConfig exts)
                    => [T.Text] -> [T.Text] -> Maybe T.Text -> ActionT exts m ()
rankingTablesAction cats ms lcs = do
    (st, ed) <- getDataStartEnd
    let key = L.toStrict . B.runPut $ do
              putCacheCategory Top
              B.put (fmap T.encodeUtf8 cats)
              B.put (fmap T.encodeUtf8 ms)
              B.put (fmap T.encodeUtf8 lcs)
              B.put (toModifiedJulianDay st)
              B.put (toModifiedJulianDay ed)
        q = RankingQuery { rankingEnd         = ed 
                         , rankingLimit       = 10
                         , rankingSince       = Nothing
                         , rankingSkip        = Nothing

                         , rankingOnlyNew     = False
                         , rankingOnlyActive  = False
                         , rankingType        = Nothing

                         , rankingPackage     = []
                         , rankingLicense     = lcs
                         , rankingCategories  = cats
                         , rankingMaintainers = ms
                         }
    (>>= lazyBytes) . C.cache key $ do
        [totRef, l1wRef, newRef, acvRef] <- replicateM 4 newEmptyMVar
        nRef <- newEmptyMVar
        _ <- fork $ rankingAction st q >>= putMVar totRef
        _ <- fork $ rankingAction st q { rankingSince      = Just $ addDays (-7) ed } >>= putMVar l1wRef
        _ <- fork $ rankingAction st q { rankingOnlyNew    = True }                   >>= putMVar newRef
        _ <- fork $ rankingAction st q { rankingOnlyActive = True, rankingSince = Just $ addDays (-31) ed } >>= putMVar acvRef
        _ <- fork $ M.access (M.count $ M.select (rankingFilter ed cats ms lcs [] False False Nothing) "packages")
            >>= putMVar nRef

        tot <- takeMVar totRef
        l1w <- takeMVar l1wRef
        new <- takeMVar newRef
        acv <- takeMVar acvRef
        n   <- takeMVar nRef

        return . A.encode $ A.object
                [ "total"      A..= (A.decode tot :: Maybe A.Value)
                , "weekly"     A..= (A.decode l1w :: Maybe A.Value)
                , "new"        A..= (A.decode new :: Maybe A.Value)
                , "active"     A..= (A.decode acv :: Maybe A.Value)
                , "nPackages"  A..= n
                , "lastUpdate" A..= UTCTime ed 0
                ]

data RankingQuery = RankingQuery
    { rankingEnd         :: Day
    , rankingLimit       :: Word
    , rankingSince       :: Maybe Day
    , rankingSkip        :: Maybe Word

    , rankingOnlyNew     :: Bool
    , rankingOnlyActive  :: Bool
    , rankingType        :: Maybe CabalType

    , rankingPackage     :: [T.Text]
    , rankingLicense     :: Maybe T.Text
    , rankingCategories  :: [T.Text]
    , rankingMaintainers :: [T.Text]
    }

instance B.Binary RankingQuery where
    get = undefined
    put (RankingQuery end lim snc skp new actv typ pkg lcs cat mnr) = do
        B.put (toModifiedJulianDay end)
        B.put lim
        B.put (fmap toModifiedJulianDay snc)
        B.put skp

        B.put new
        B.put actv
        B.put typ

        B.put (fmap T.encodeUtf8 pkg)
        B.put (fmap T.encodeUtf8 lcs)
        B.put (fmap T.encodeUtf8 cat)
        B.put (fmap T.encodeUtf8 mnr)

rankingQuery ::  MonadIO m => RankingQuery -> M.Action m [M.Document]
rankingQuery RankingQuery{..} = M.aggregate "packages" $ case rankingSince of
    Just since ->
        (if null filt then id else (:) ["$match" M.=: filt]) $.
        ["$project" M.=: [ "recent"      M.=: M.Int64 1
                         , "synopsis"    M.=: M.Int64 1
                         , "author"      M.=: M.Int64 1
                         , "maintainers" M.=: M.Int64 1
                         , "name"        M.=: M.Int64 1
                         , "category"    M.=: M.Int64 1
                         ]] :
        [ "$unwind" M.=: ("$recent" :: T.Text)] :
        [ "$match"  M.=: ["recent.date"  M.=: ["$gt" M.=: M.UTC (UTCTime since 0)]]] :
        ["$group"   M.=: [ "_id"         M.=: ("$name" :: T.Text)
                         , "name"        M.=: ["$first" M.=: ("$name"         :: T.Text)]
                         , "synopsis"    M.=: ["$first" M.=: ("$synopsis"     :: T.Text)]
                         , "author"      M.=: ["$first" M.=: ("$author"       :: T.Text)]
                         , "maintainers" M.=: ["$first" M.=: ("$maintainers"  :: T.Text)]
                         , "category"    M.=: ["$first" M.=: ("$category"     :: T.Text)]
                         , "total"       M.=: ["$sum"   M.=: ("$recent.count" :: T.Text)]
                         ]] :
        ["$sort"    M.=: ["total" M.=: (-1 :: Int)]] :
        maybe id (\skp -> (:) ["$skip" M.=: (fromIntegral skp :: Int)]) rankingSkip $.
        ["$limit"   M.=: M.Int64 (fromIntegral $ min 100 rankingLimit)] : []
    Nothing ->
        (if null filt then id else (["$match" M.=: filt]:)) $.
        ["$project" M.=: [ "total"       M.=: M.Int64 1
                         , "synopsis"    M.=: M.Int64 1
                         , "author"      M.=: M.Int64 1
                         , "maintainers" M.=: M.Int64 1
                         , "category"    M.=: M.Int64 1
                         , "name"        M.=: M.Int64 1
                         ]] :
        ["$sort"    M.=: ["total" M.=: (-1 :: Int)]] :
        maybe id (\skp -> (:) ["$skip" M.=: (fromIntegral skp :: Int)]) rankingSkip $.
        ["$limit"   M.=: M.Int64 (fromIntegral rankingLimit)] : []
  where
    filt = rankingFilter rankingEnd rankingCategories rankingMaintainers rankingLicense rankingPackage rankingOnlyActive rankingOnlyNew rankingType

rankingFilter :: Day -> [T.Text] -> [T.Text] -> Maybe T.Text -> [T.Text] -> Bool -> Bool -> Maybe CabalType -> M.Document
rankingFilter end cats ms lcs pkg actv new typ =
    (if null cats then id else (:) ("$and" M.=: map (\c -> ["category" M.=: c]) cats)) $
    (if null ms   then id else (:) ("maintainers"    M.=: ["$in" M.=: ms])) $
    (case lcs of {Nothing -> id; Just l -> (("license" M.=: l):)}) $
    (if null pkg then id else (:) ("$and" M.=: map (\p -> ["name" M.=: M.Regex p "i"]) pkg)) $
    (if not new  then id else (:) ("initialRelease" M.=: ["$gt" M.=: UTCTime (addDays (-31) end) 0])) $
    (if not actv then id else (:) ("lastRelease"    M.=: ["$gt" M.=: UTCTime (addDays (-31) end) 0])) $
    case typ of
        Nothing         -> []
        Just Library    -> ["hasLibrary"  M.=: True]
        Just Executable -> ["executables" M.=: ["$ne" M.=: ([] :: [T.Text])]]
