{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.ByteString.Char8 as S8

import Web.Apiary
import Network.Wai(Application)
import Network.Wai.Handler.Warp

import Data.Word
import Data.Pool
import Data.Typeable
import Data.Maybe
import Data.Time
import Data.Reflection
import qualified Data.Aeson as JSON

import qualified Database.MongoDB as Mongo

import Common

getPortEnv :: IO Int
getPortEnv = 
    handle (\(_ :: SomeException) -> return 3000) (read <$> getEnv "PORT")

data MongoState = MongoState
    { mongoPool     :: Pool Mongo.Pipe
    , mongoUser     :: T.Text
    , mongoPassword :: T.Text
    , mongoDB       :: T.Text
    }

main :: IO ()
main = do
    port <- getPortEnv
    (muser, mpasswd, mhost, mport, mdb) <- getMongoConfig
    let create = Mongo.connect' 6 (Mongo.Host mhost $ Mongo.PortNumber $ fromIntegral mport)
    
    pool <- createPool create Mongo.close 1 20 5

    run port $ application muser mpasswd mdb pool

access' :: (MonadIO m, MonadBaseControl IO m) => MongoState -> Mongo.Action m a -> m a
access' mst m = withResource (mongoPool mst) $ \pipe ->
    Mongo.access pipe Mongo.master (mongoDB mst) m

access :: (MonadIO m, MonadBaseControl IO m, Given MongoState) => Mongo.Action m a -> m a
access = access' given

auth :: (MonadIO m, Given MongoState) => Mongo.Action m ()
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

-- member
-- library
-- executable
-- license
-- executables
-- category

rankingAction :: (MonadIO m, MonadBaseControl IO m, Given MongoState)
              => Maybe Word -> Maybe Word -> Maybe Since -> ActionT m ()
rankingAction mblimit mbrange mbsince = do
    contentType "application/json"

    let limit = maybe 10 (min 100 . fromIntegral) mblimit

    since <- case mbsince of
        Just s  -> return $ getDay s
        Nothing ->
            addDays (negate . fromIntegral $ maybe 7 (min 100) mbrange)
            . utctDay <$> liftIO getCurrentTime

    doc <- access $ do
        auth
        rank <- Mongo.aggregate "downloads"
            [aggrSinceDate since, aggrGroupByPackage, aggrSort, aggrLimit limit]
        catMaybes <$> mapM (\d -> fmap (Mongo.merge d) <$>
            Mongo.findOne (Mongo.select ["_id" Mongo.=: Mongo.valueAt "_id" d] "cabal")) rank

    lazyBytes . JSON.encode $ map (toAeson . Mongo.include cabalFields) doc


application :: Mongo.Username -> Mongo.Password -> T.Text -> Pool Mongo.Pipe -> Application
application muser mpasswd mdb pool = runApiary def $ give (MongoState pool muser mpasswd mdb) $ do
    [capture|/ranking|] . method GET $ do
        eqHeader "Accept" "application/json"
            . ("limit" ?? "number to fetch ranking(default: 10)."  =?: pWord) 
            . ("range" ?? "days of aggregation(default: 7)."       =?: pWord)
            . ("since" ?? "date of aggregation. instead of range." =?: (Proxy :: Proxy Since))
            . document "get package download ranking."
            $ action rankingAction

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

aggrSinceDate :: Day -> Mongo.Document
aggrSinceDate since = [ "$match" Mongo.=: 
    [ "date" Mongo.=: [ "$gte" Mongo.=: Mongo.UTC (UTCTime since 0)] ] ]

aggrGroupByPackage :: Mongo.Document
aggrGroupByPackage = [ "$group" Mongo.=:
    [ "_id"   Mongo.=: Mongo.String "$package"
    , "total" Mongo.=: ["$sum" Mongo.=: Mongo.String "$count"]
    ] ]

aggrSort :: Mongo.Document
aggrSort = ["$sort" Mongo.=: ["total" Mongo.=: Mongo.Int64 (-1)]]

aggrLimit :: Int -> Mongo.Document
aggrLimit lim = ["$limit" Mongo.=: Mongo.Int64 (fromIntegral lim)]
