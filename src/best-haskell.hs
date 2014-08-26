{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Exception
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Control

import System.Environment

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as L

import Web.Apiary
import Network.Wai.Handler.Warp

import Data.Binary.Put
import Data.Word
import Data.Char
import Data.Pool
import Data.Typeable
import Data.Maybe
import Data.Time
import Data.Reflection
import qualified Data.Aeson as JSON

import qualified Database.MongoDB as Mongo
import qualified Database.Memcached.Binary.Maybe as Mc

import Common

getPortEnv :: IO Int
getPortEnv = 
    handle (\(_ :: SomeException) -> return 3000) (read <$> getEnv "PORT")

data AppState = AppState
    { mongoPool     :: Pool Mongo.Pipe
    , mongoUser     :: T.Text
    , mongoPassword :: T.Text
    , mongoDB       :: T.Text
    , memcachedConn :: Mc.Connection
    }

main :: IO ()
main = do
    port <- getPortEnv
    (muser, mpasswd, mhost, mport, mdb) <- getMongoConfig
    let create = Mongo.connect' 20 (Mongo.Host mhost $ Mongo.PortNumber $ fromIntegral mport)
    pool <- createPool create Mongo.close 1 20 5

    (chost, cport, cuser, cpasswd) <- getMemcachedConfig
    mc <- Mc.connect def
        { Mc.numConnection = 10
        , Mc.connectHost   = T.unpack chost
        , Mc.connectPort   = Mc.PortNumber $ fromIntegral cport
        , Mc.connectAuth   = [Mc.Plain (T.encodeUtf8 cuser) (T.encodeUtf8 cpasswd)]
        }

    run port . runApiary def $ give (AppState pool muser mpasswd mdb mc) application

access' :: (MonadIO m, MonadBaseControl IO m) => AppState -> Mongo.Action m a -> m a
access' mst m = withResource (mongoPool mst) $ \pipe ->
    Mongo.access pipe Mongo.master (mongoDB mst) m

access :: (MonadIO m, MonadBaseControl IO m, Given AppState) => Mongo.Action m a -> m a
access = access' given

auth :: (MonadIO m, Given AppState) => Mongo.Action m ()
auth = do
    b <- Mongo.auth (mongoUser given) (mongoPassword given)
    unless b $ fail "MongoDB auth failed."

newtype Since = Since {getDay :: Day} deriving (Typeable, Show)

instance Query Since where
    readQuery = (>>= \s0 -> do
        (y, s1) <- S8.readInt s0
        (m, s2) <- S8.readInt (S8.tail s1)
        (d,  _) <- S8.readInt (S8.tail s2)
        let y' = if y < 100 then 2000 + y else y
        return . Since $ fromGregorian (fromIntegral y') m d)
    qTypeRep _ = typeOf (undefined :: Day)

data CabalType = Executable | Library deriving (Typeable, Show)
instance Path CabalType where
    readPath s = case T.toLower s of
        sl | sl == "library"    -> Just Library
           | sl == "executable" -> Just Executable
           | otherwise -> Nothing

instance Query CabalType where
    readQuery (Just s) = case S8.map toLower s of
        sl | sl `elem` ["l", "lib", "library"]    -> Just Library
           | sl `elem` ["e", "exe", "executable"] -> Just Executable
           | otherwise -> Nothing
    readQuery Nothing = Nothing

mkCacheKey :: Maybe Word -> Maybe Day -> [T.Text] -> [T.Text] -> Maybe CabalType -> S8.ByteString
mkCacheKey lim snc cat mem typ = L.toStrict . runPut $ do
    putByteString "ranking/"
    maybe (return ()) (putWord64be . fromIntegral) lim
    putWord8 47 -- '/'
    maybe (return ()) ((\(y,m,d) -> do
        putWord16be (fromIntegral y)
        putWord8    (fromIntegral m)
        putWord8    (fromIntegral d)
        ) . toGregorian) snc
    putWord8 47 -- '/'
    mapM_ (putByteString . T.encodeUtf8) cat
    putWord8 47 -- '/'
    mapM_ (putByteString . T.encodeUtf8) mem
    putWord8 47 -- '/'
    case typ of
        Nothing         -> putWord8 0
        Just Library    -> putWord8 1
        Just Executable -> putWord8 2

rankingAction :: (MonadIO m, MonadBaseControl IO m, Given AppState)
              => Maybe Word -> Maybe Day -> [T.Text] -> [T.Text] -> Maybe CabalType -> ActionT m ()
rankingAction mblimit mbsince cats mems typ = do
    contentType "application/json"

    let cacheKey = mkCacheKey mblimit mbsince cats mems typ

    liftIO (Mc.get_ cacheKey (memcachedConn given)) >>= \case
        Nothing -> do
            let limit = maybe 10 (min 100 . fromIntegral) mblimit

            doc <- access $ do
                auth
                oids <- getOids cats mems typ

                rank <- Mongo.aggregate "downloads" $
                    aggrSinceDateOids mbsince oids :
                    [aggrGroupByPackage, aggrSort, aggrLimit limit]
                catMaybes <$> mapM (\d -> fmap (Mongo.merge d) <$>
                    Mongo.findOne (Mongo.select ["_id" Mongo.=: Mongo.valueAt "_id" d] "cabal")) rank

            let rb = JSON.encode $ map (toAeson . Mongo.include cabalFields) doc

            void . liftIO $ Mc.set 0 0 cacheKey rb (memcachedConn given)

            lazyBytes rb

        Just c -> lazyBytes c
  where
    ctCond mbtyp = case mbtyp of
        Nothing         -> id
        Just Library    -> (("has_library" Mongo.=: True):)
        Just Executable -> (("executables" Mongo.=: ["$ne" Mongo.=: ([] :: [Mongo.Value])] ):)
    getOids []  []  Nothing = return Nothing
    getOids cat mem mbtyp   = fmap (Just . map (\d -> Mongo.at "_id" d :: Mongo.ObjectId)) $ Mongo.rest =<<
        Mongo.find (Mongo.select selDoc "cabal") { Mongo.project = ["_id" Mongo.=: (1 :: Int)] }
      where
        selDoc  = ctCond mbtyp $ kv "members" mem $ kv "category" cat []
        kv _ [] = id
        kv k vs = ((k Mongo.=: ["$all" Mongo.=: vs]):)

mkSince :: Maybe Word -> Maybe Since -> IO (Maybe Day)
mkSince mbrange mbsince = case mbsince of
    Just s  -> return . Just $ getDay s
    Nothing -> case mbrange of
        Nothing -> return Nothing
        Just r  -> Just . addDays (negate $ fromIntegral r) . utctDay <$> getCurrentTime

application :: Given AppState => Apiary '[] ()
application = do
    ("limit" ?? "number to fetch ranking(default: 10)."  =?: pWord) 
        . ("range" ?? "days of aggregation."                   =?: pWord)
        . ("since" ?? "date of aggregation. instead of range." =?: (Proxy :: Proxy Since)) $ do

        [capture|/ranking|]
            . ("category" ?? "category filter."       =*: pText)
            . ("member"   ?? "member filter."         =*: pText)
            . ("type"     ?? "type of package(executable or library)" =?: (Proxy :: Proxy CabalType)) $ do

            eqHeader "Accept" "application/json" . method GET 
                . document "get package download ranking."
                . action $ \mblimit mbrange mbsince cats mems typ -> do
                    since <- liftIO $ mkSince mbrange mbsince
                    rankingAction mblimit since cats mems typ

        [capture|/ranking/category/:T.Text[category]|] $ do
            eqHeader "Accept" "application/json" . method GET
            . document "get ranking par category."
            . action $ \mblimit mbrange mbsince cat -> do
                since <- liftIO $ mkSince mbrange mbsince
                rankingAction mblimit since [cat] [] Nothing

        [capture|/ranking/member/:T.Text[member]|] $ do
            eqHeader "Accept" "application/json" . method GET
            . document "get ranking par member."
            . action $ \mblimit mbrange mbsince mem -> do
                since <- liftIO $ mkSince mbrange mbsince
                rankingAction mblimit since [] [mem] Nothing

        [capture|/ranking/:CabalType[library or executable]|] $ do
            eqHeader "Accept" "application/json" . method GET
            . document "get ranking par type."
            . action $ \mblimit mbrange mbsince ct -> do
                since <- liftIO $ mkSince mbrange mbsince
                rankingAction mblimit since [] [] (Just ct)

    [capture|/package/:T.Text[package name]|] . method GET $ do
        eqHeader "Accept" "application/json"
            . document "get package information."
            . action $ \pkg -> do
                join . access $ do
                    auth
                    Mongo.findOne (Mongo.select ["package" Mongo.=: pkg] "cabal") >>= \case
                        Nothing    -> return $
                            status status404 >> bytes "unknown package" >> stop

                        Just pinfo -> do
                            ds <- Mongo.rest =<< Mongo.find 
                                (Mongo.select ["package" Mongo.=: Mongo.valueAt "_id" pinfo] "downloads")
                                {Mongo.project = [ "count" Mongo.=: Mongo.Int64 1
                                                 , "date"  Mongo.=: Mongo.Int64 1
                                                 ]}
                            let ds' = map (Mongo.include ["date", "count"]) ds
                            return $ do
                                contentType "application/json"
                                let doc = ("downloads" Mongo.=: ds') : pinfo
                                lazyBytes . JSON.encode $ toAeson doc

    [capture|/api/documentation|] . method GET . action $
        defaultDocumentationAction def

cabalFields :: [Mongo.Label]
cabalFields = [ "homepage", "copyright", "category"
              , "members", "maintainer", "version"
              , "synopsis", "package", "has_library"
              , "total", "stability", "author"
              , "bugReports", "license", "executables"
              , "description"
              ]

aggrSinceDateOids :: Maybe Day -> Maybe [Mongo.ObjectId] -> Mongo.Document
aggrSinceDateOids mbsince mboids = [ "$match" Mongo.=:
    (maybe id (\since -> (("date"    Mongo.=: ["$gte" Mongo.=: Mongo.UTC (UTCTime since 0)]):)) mbsince .
     maybe id (\oids ->  (("package" Mongo.=: ["$in"  Mongo.=: oids]):)) mboids) []
    ]

aggrGroupByPackage :: Mongo.Document
aggrGroupByPackage = [ "$group" Mongo.=:
    [ "_id"   Mongo.=: Mongo.String "$package"
    , "total" Mongo.=: ["$sum" Mongo.=: Mongo.String "$count"]
    ] ]

aggrSort :: Mongo.Document
aggrSort = ["$sort" Mongo.=: ["total" Mongo.=: Mongo.Int64 (-1)]]

aggrLimit :: Int -> Mongo.Document
aggrLimit lim = ["$limit" Mongo.=: Mongo.Int64 (fromIntegral lim)]
