{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Exception
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Control

import System.Environment

import qualified Data.Text as T

import Web.Apiary
import Network.Wai(Application)
import Network.Wai.Handler.Warp

import Data.Pool
import Data.Time
import Data.Int
import Data.Reflection

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

application :: Mongo.Username -> Mongo.Password -> T.Text -> Pool Mongo.Pipe -> Application
application muser mpasswd mdb pool = runApiary def $ give (MongoState pool muser mpasswd mdb) $ do
    root . action $ do
        doc <- access $ do
            auth
            let day = ModifiedJulianDay 56886
            Mongo.aggregate "downloads" $ pipeline day 10
        contentType "text/html"
        mapM_ (\a -> showing (toAeson a) >> char '\n') doc

pipeline :: Day -> Int64 -> Mongo.Pipeline
pipeline since num = [match, grouping, sorting, limit]
  where
    match =
        [ "$match" Mongo.=: 
            [ "date" Mongo.=: [ "$gt" Mongo.=: Mongo.UTC (UTCTime since 0)] ]
        ]
    grouping =
        [ "$group" Mongo.=:
            [ "_id"   Mongo.=: Mongo.String "$package"
            , "total" Mongo.=: ["$sum" Mongo.=: Mongo.String "$count"]
            ]
        ]
    sorting = ["$sort" Mongo.=: ["total" Mongo.=: Mongo.Int64 (-1)]]
    limit   = ["$limit" Mongo.=: Mongo.Int64 num]
