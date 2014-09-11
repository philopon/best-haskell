{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import System.Environment
import System.Process
import System.Exit

import Web.Apiary
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Autohead
import Network.Wai.Handler.Warp
import qualified Web.Apiary.MongoDB   as M
import qualified Web.Apiary.Memcached as C
import qualified Database.Memcached.Binary.Maybe as C

import Control.Monad
import Control.Monad.Trans.Control
import Control.Applicative
import Control.Exception
import Control.Concurrent

import qualified Data.Binary as B
import qualified Data.Binary.Put as B
import qualified Data.Aeson as A
import Data.Abeson
import Data.Apiary.Extension
import Data.Word
import Data.Maybe
import Data.Char
import Data.Typeable
import Data.Time
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as T
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as L

-- | database configuration
-- if mongoEnvName/memcachedenvprefix environment variable exists, get it,
-- else heroku config:get env --app herokuAppName.
herokuAppName, mongoEnvName, memcachedEnvPrefix :: String
herokuAppName      = "best-haskell"
mongoEnvName       = "MONGOHQ_URL"
memcachedEnvPrefix = "MEMCACHIER"

getHerokuConfig :: String -> IO T.Text
getHerokuConfig key = do
    (_, Just stdout, _, h) <- createProcess
        (proc "heroku" ["config:get", key, "--app", herokuAppName]) {std_out = CreatePipe}
    xc <- waitForProcess h
    if xc == ExitSuccess
        then T.hGetLine stdout
        else fail "heroku command failure."

getEnvOrHerokuConfig :: String -> IO T.Text
getEnvOrHerokuConfig ev = handle (\(_::SomeException) -> getHerokuConfig ev) $ T.pack <$> getEnv ev

getMongoDBConfig :: String -> IO (T.Text, T.Text, String, Int, T.Text)
getMongoDBConfig ev = do
    s0 <- getEnvOrHerokuConfig ev
    let (_,      s1)    = T.breakOnEnd "://" s0
        (user,   s2) = T.break (== ':') s1
        (passwd, s3) = T.break (== '@') (T.tail s2)
        (host_,  s4) = T.break (== ':') (T.tail s3)
        (port,   s5) = T.break (== '/') (T.tail s4)
    return (user, passwd, T.unpack host_, read $ T.unpack port, (if T.null s5 then id else T.tail) s5)

getMemcachedConfig' :: String -> IO (T.Text, Int, T.Text, T.Text)
getMemcachedConfig' evpfx = do
    (s,p) <- T.break (== ':') <$> getEnvOrHerokuConfig (evpfx ++ "_SERVERS")
    usr   <- getEnvOrHerokuConfig (evpfx ++ "_USERNAME")
    pwd   <- getEnvOrHerokuConfig (evpfx ++ "_PASSWORD")
    return (s, read $ T.unpack $ T.tail p, usr, pwd)

getMemcachedConfig :: IO (T.Text, Int, T.Text, T.Text)
getMemcachedConfig = getMemcachedConfig' memcachedEnvPrefix

-- | global application config
data AppConfig = AppConfig { cacheEnable        :: Bool
                           , flushHandlerEnable :: Bool
                           , cacheTime          :: Int
                           , startEndCache      :: MVar (UTCTime, UTCTime)
                           }

-- | set configuration and run server with extensions.
serv :: (Extensions '[AppConfig, C.Memcached, M.MongoDB] -> IO Application) -> IO ()
serv app = do
    port:_ <- getArgs
    (mu,mw,mh,mp,md) <- getMongoDBConfig mongoEnvName
    (cs, cp, cu, cw) <- getMemcachedConfig
    let mongoConf = def { M.mongoDBHost       = M.Host mh (M.PortNumber $ fromIntegral mp)
                        , M.mongoDBAuth       = Just (mu, mw)
                        , M.mongoDBDatabase   = md
                        , M.mongoDBTimeout    = 30
                        , M.mongoDBAccessMode = M.slaveOk
                        , M.numConnection     = 10
                        }
        mcConf    = def { C.connectHost   = T.unpack cs
                        , C.connectPort   = C.PortNumber $ fromIntegral cp
                        , C.connectAuth   = [C.Plain (T.encodeUtf8 cu) (T.encodeUtf8 cw)]
                        , C.numConnection = 10
                        }
    ac <- AppConfig True True (600 * 10^(6::Int)) <$> newEmptyMVar
    serverWith (M.initMongoDB mongoConf +> C.initMemcached mcConf +> initializer (return ac)) (run $ read port) app

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
    --middleware $ gzip def
    --middleware autohead

    -- API

    -- global options
    ("limit" ?? "number to fetch ranking."                     =?!: (10 :: Word))
        . ("range" ?? "days of aggregation."                   =?:  pWord)
        . ("since" ?? "date of aggregation. instead of range." =?:  (Proxy :: Proxy Day)) $ do

        -- /ranking most generic ranking api 
        [capture|/ranking|]
            . ("category" ?? "category filter."       =*: pText)
            . ("type"     ?? "type of package(executable or library)" =?: (Proxy :: Proxy CabalType)) $ do

            accept "application/json" . method GET 
                . document "get package download ranking."
                . action $ \limit mbrange mbsince cats typ -> do
                    (st, ed) <- getDataStartEnd
                    rankingAction st ed limit (mkSince (utctDay ed) mbrange mbsince) cats typ >>= lazyBytes

        -- /ranking/category/:category category specific raanking api
        [capture|/ranking/category/:T.Text|]
            . ("type"     ?? "type of package(executable or library)" =?: (Proxy :: Proxy CabalType)) $ do

            accept "application/json" . method GET 
                . document "get package download ranking."
                . action $ \limit mbrange mbsince cat typ -> do
                    (st, ed) <- getDataStartEnd
                    rankingAction st ed limit (mkSince (utctDay ed) mbrange mbsince) [cat] typ >>= lazyBytes

    -- /categories list up category
    [capture|/categories|] . accept "application/json" . method GET . action $ categoriesAction

    [capture|/package/:T.Text|] . accept "application/json" . method GET . action $ \pkg ->
        M.access (M.findOne (M.select ["name" M.=: pkg] "packages") {
            M.project = ["_id" M.=: (0::Int), "recent" M.=: (0::Int)]
            }) >>= \case
            Nothing -> status status404 >> bytes "package not found." >> stop
            Just p  -> lazyBytes . A.encode $ toAeson def p

    -- /flush flushAll memcached
    ac <- apiaryExt (Proxy :: Proxy AppConfig)
    when (flushHandlerEnable ac) $ [capture|/flush|] . method GET . action $ do
        _ <- C.memcached C.flushAll
        bytes "flush"

    -- / all information used in root page.
    [capture|/|] . accept "application/json" . method GET . action $ rootJsAction

    -- static files
    root                     . method GET . action $ file "static/main.html"         Nothing
    [capture|/view/:String|] . method GET . action $ \f -> file ("static/view/" ++ f) Nothing
    [capture|/img/:String|]  . method GET . action $ \f -> file ("static/img/"  ++ f) Nothing
    [capture|/:String|]      . method GET . action $ \f -> file ("static/"      ++ f) Nothing

    -- other
    [capture|/nop|] . method GET . action $ bytes "nop\n"
    [capture|/api/documentation|] . method GET . action $ defaultDocumentationAction def

mkSince :: Day -> Maybe Word -> Maybe Day -> Maybe Day
mkSince end mbrange mbsince = case mbsince of
    Just s  -> Just s
    Nothing -> case mbrange of
        Nothing -> Nothing
        Just r  -> Just $ addDays (negate $ fromIntegral r) end

infixr 5 &
(&) :: a -> a
(&) = id

memcacheRankingKey :: UTCTime -> UTCTime -> Word -> Maybe Day -> [T.Text] -> Maybe CabalType -> S8.ByteString
memcacheRankingKey (UTCTime st _) (UTCTime ed _) lim mbsince cat typ = L.toStrict . B.runPut $ do
    B.putByteString "r"
    B.put (toModifiedJulianDay st)
    B.put (toModifiedJulianDay ed)
    B.put lim
    B.put (maybe 0 toModifiedJulianDay mbsince)
    mapM_ (B.put . T.encodeUtf8) cat
    B.put $ case typ of { Nothing -> 0; Just Library -> 1; Just Executable -> (2::Word8) }

memcacheCategoriesKey :: UTCTime -> UTCTime -> S8.ByteString
memcacheCategoriesKey (UTCTime st _) (UTCTime ed _) = L.toStrict . B.runPut $ do
    B.putByteString "c"
    B.put (toModifiedJulianDay st)
    B.put (toModifiedJulianDay ed)

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

cache :: (Has AppConfig exts, Has C.Memcached exts, MonadIO m)
      => C.Key -> ActionT exts m C.Value -> ActionT exts m C.Value
cache key a = do
    doCache <- cacheEnable <$> getExt (Proxy :: Proxy AppConfig)
    (if doCache then C.memcached (C.get_ key) else return Nothing) >>= \case
        Just c  -> return c
        Nothing -> do
            r <- a
            when doCache . void $ C.memcached $ C.set 0 0 key r
            return r

rankingAction :: (MonadIO m, MonadBaseControl IO m, Has M.MongoDB exts, Has C.Memcached exts, Has AppConfig exts)
              => UTCTime -> UTCTime -> Word -> Maybe Day -> [T.Text] -> Maybe CabalType -> ActionT exts m L.ByteString
rankingAction st ed limit mbsince cats typ = do
    let key = memcacheRankingKey st ed limit mbsince cats typ
    cache key $ do
        doc <- M.access $ rankingQuery limit mbsince cats typ
        return . A.encode $ A.object [ "ranking" A..= map (toAeson def . filter (\(l M.:= _) -> l /= "_id")) doc
                                     , "start"   A..= fmap (max st . flip UTCTime 0) mbsince
                                     , "end"     A..= ed
                                     ]

rootJsAction :: (MonadIO m, MonadBaseControl IO m, Has M.MongoDB exts, Has C.Memcached exts, Has AppConfig exts)
             => ActionT exts m ()
rootJsAction = do
    (st, ed) <- getDataStartEnd
    let key = L.toStrict . B.runPut $
              B.putByteString "t" >> B.put (toModifiedJulianDay $ utctDay ed)
    r <- cache key $ do
        tot <- rankingAction st ed 10 Nothing [] Nothing
        l1w <- rankingAction st ed 10 (Just $ addDays (-7)  $ utctDay ed) [] Nothing
        l1m <- rankingAction st ed 10 (Just $ addDays (-28) $ utctDay ed) [] Nothing
        n   <- M.access (M.count $ M.select [] "packages")
        return . A.encode $ A.object
                [ "total"      A..= (A.decode tot :: Maybe A.Value)
                , "weekly"     A..= (A.decode l1w :: Maybe A.Value)
                , "monthly"    A..= (A.decode l1m :: Maybe A.Value)
                , "nPackages"  A..= n
                , "lastUpdate" A..= ed
                ]
    lazyBytes r


categoriesAction :: (MonadIO m, MonadBaseControl IO m, Has C.Memcached exts, Has M.MongoDB exts, Has AppConfig exts)
                 => ActionT exts m ()
categoriesAction = do
    (st, ed) <- getDataStartEnd
    let key = memcacheCategoriesKey st ed
    r <- cache key $ do
            doc <- M.access $ M.aggregate "packages"
                [ ["$project" M.=: ["category" M.=: (1 :: Int)]]
                , ["$unwind"  M.=: ("$category" :: T.Text)]
                , ["$group"   M.=: ["_id" M.=: ("$category" :: T.Text), "count" M.=: ["$sum" M.=: (1::Int)]]]
                ]
            return . A.encode $ A.object [ "categories" A..= map conv doc
                                         , "latest"     A..= ed
                                         ]
    lazyBytes r
  where
    conv b = A.object [ "category" A..= (M.at "_id"   b :: T.Text)
                      , "count"    A..= (M.at "count" b :: Int)
                      ]

rankingQuery :: MonadIO m => Word -> Maybe Day -> [T.Text] -> Maybe CabalType -> M.Action m [M.Document]
rankingQuery limit mbsince cats typ = M.aggregate "packages" $ case mbsince of
    Just since ->
        (if null cats && isNothing typ then id else (["$match" M.=: filt]:)) &
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
        (if null cats && isNothing typ then id else (["$match" M.=: filt]:)) &
        ["$project" M.=: [ "total"    M.=: M.Int64 1
                         , "synopsis" M.=: M.Int64 1
                         , "author"   M.=: M.Int64 1
                         , "category" M.=: M.Int64 1
                         , "name"     M.=: M.Int64 1
                         ]] :
        ["$sort"    M.=: ["total" M.=: (-1 :: Int)]] :
        ["$limit"   M.=: M.Int64 (fromIntegral limit)] : []
        
  where
    filt = (if null cats then id else (("category" M.=: ["$in" M.=: cats]):)) $ case typ of
        Nothing         -> []
        Just Library    -> ["hasLibrary"  M.=: True]
        Just Executable -> ["executables" M.=: ["$ne" M.=: ([] :: [T.Text])]]
