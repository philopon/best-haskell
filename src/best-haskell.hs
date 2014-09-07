{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

import System.Environment
import System.Process
import System.Exit

import Web.Apiary
import Network.Wai.Handler.Warp
import qualified Web.Apiary.MongoDB   as M
import qualified Web.Apiary.Memcached as C
import qualified Database.Memcached.Binary.Maybe as C

import Control.Monad.Trans.Control
import Control.Applicative
import Control.Exception

import qualified Data.Binary as B
import qualified Data.Binary.Put as B
import qualified Data.Aeson as A
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

herokuAppName, mongoEnvName :: String
herokuAppName = "best-haskell"
mongoEnvName = "MONGOHQ_URL"

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
getMemcachedConfig = getMemcachedConfig' "MEMCACHIER"

serv :: (Extensions '[C.Memcached, M.MongoDB] -> IO Application) -> IO ()
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
    serverWith (M.initMongoDB mongoConf +> C.initMemcached mcConf) (run $ read port) app

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
    ("limit" ?? "number to fetch ranking."                     =?!: (10 :: Word))
        . ("range" ?? "days of aggregation."                   =?:  pWord)
        . ("since" ?? "date of aggregation. instead of range." =?:  (Proxy :: Proxy Day)) $ do

        [capture|/ranking|]
            . ("category" ?? "category filter."       =*: pText)
            . ("type"     ?? "type of package(executable or library)" =?: (Proxy :: Proxy CabalType)) $ do

            accept "application/json" . method GET 
                . document "get package download ranking."
                . action $ \limit mbrange mbsince cats typ -> do
                    since <- liftIO $ mkSince mbrange mbsince
                    rankingAction limit since cats typ

mkSince :: Maybe Word -> Maybe Day -> IO (Maybe Day)
mkSince mbrange mbsince = case mbsince of
    Just s  -> return $ Just s
    Nothing -> case mbrange of
        Nothing -> return Nothing
        Just r  -> Just . addDays (negate $ fromIntegral r) . utctDay <$> getCurrentTime

infixr 5 &
(&) :: a -> a
(&) = id

memcacheKey :: UTCTime -> UTCTime -> Word -> Maybe Day -> [T.Text] -> Maybe CabalType -> S8.ByteString
memcacheKey (UTCTime st _) (UTCTime ed _) lim mbsince cat typ = L.toStrict . B.runPut $ do
    B.put $ toModifiedJulianDay st
    B.putWord8 0
    B.put $ toModifiedJulianDay ed
    B.putWord8 0
    B.put lim
    B.putWord8 0
    B.put (maybe 0 toModifiedJulianDay mbsince)
    B.putWord8 0
    mapM_ (B.put . T.encodeUtf8) cat
    B.putWord8 0
    B.put $ case typ of { Nothing -> 0; Just Library -> 1; Just Executable -> (2::Word8) }

rankingAction :: (MonadIO m, MonadBaseControl IO m, Has M.MongoDB exts, Has C.Memcached exts)
              => Word -> Maybe Day -> [T.Text] -> Maybe CabalType -> ActionT exts m ()
rankingAction limit mbsince cats typ = do
    (Just st', Just ed') <- M.access $ (,)
        <$> M.findOne (M.select ["key" M.=: ("recent_start" :: T.Text)] "config")
        <*> M.findOne (M.select ["key" M.=: ("last_update"  :: T.Text)] "config")
    let st  = M.at "value" st'
        ed  = M.at "value" ed'
        key = memcacheKey st ed limit mbsince cats typ
    C.memcached (C.get_ key) >>= \case
        Nothing -> do
            doc <- M.access $ M.aggregate "packages" (aggrQuery limit mbsince cats typ)
            let ret = A.encode $ A.object [ "ranking" A..= map aggrToValue doc
                                          , "start"   A..= fmap (max st . flip UTCTime 0) mbsince
                                          , "end"     A..= ed
                                          ]
            _ <- C.memcached $ C.set 0 0 key ret
            lazyBytes ret
        Just ret -> do
            lazyBytes ret

aggrToValue :: M.Document -> A.Value
aggrToValue bson = A.object [ "name"     A..= (M.at "name"     bson :: T.Text)
                            , "synopsis" A..= (M.at "synopsis" bson :: T.Text)
                            , "author"   A..= (M.at "author"   bson :: T.Text)
                            , "total"    A..= (M.at "total"    bson :: Int)
                            ]

aggrQuery :: Word -> Maybe Day -> [T.Text] -> Maybe CabalType -> M.Pipeline
aggrQuery limit mbsince cats typ = case mbsince of
    Just since ->
        (if null cats && isNothing typ then id else (["$match" M.=: filt]:)) &
        ["$project" M.=: [ "recent"    M.=: M.Int64 1
                         , "synopsis"  M.=: M.Int64 1
                         , "author"    M.=: M.Int64 1
                         , "name"      M.=: M.Int64 1
                         ]] :
        [ "$match"  M.=: ["recent.date" M.=: ["$gt" M.=: M.UTC (UTCTime since 0)]]] :
        [ "$unwind" M.=: ("$recent" :: T.Text)] :
        ["$group"   M.=: [ "_id"      M.=: ("$name" :: T.Text)
                         , "name"     M.=: ["$first" M.=: ("$name"         :: T.Text)]
                         , "synopsis" M.=: ["$first" M.=: ("$synopsis"     :: T.Text)]
                         , "author"   M.=: ["$first" M.=: ("$author"       :: T.Text)]
                         , "total"    M.=: ["$sum"   M.=: ("$recent.count" :: T.Text)]
                         ]] :
        ["$sort"    M.=: ["total" M.=: (-1 :: Int)]] :
        ["$limit"   M.=: M.Int64 (fromIntegral $ min 100 limit)] : []
    Nothing ->
        (if null cats && isNothing typ then id else (["$match" M.=: filt]:)) &
        ["$project" M.=: [ "total"    M.=: M.Int64 1
                         , "synopsis" M.=: M.Int64 1
                         , "author"   M.=: M.Int64 1
                         , "name"     M.=: M.Int64 1
                         ]] :
        ["$sort"    M.=: ["total" M.=: (-1 :: Int)]] :
        ["$limit"   M.=: M.Int64 (fromIntegral limit)] : []
        
  where
    filt = (if null cats then id else (("category" M.=: cats):)) $ case typ of
        Nothing         -> []
        Just Library    -> ["hasLibrary"  M.=: True]
        Just Executable -> ["executables" M.=: ["$ne" M.=: ([] :: [T.Text])]]
