{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
import System.Environment
import System.IO

import Control.Applicative
import  Control.Monad
import  Control.Monad.Trans
import  Control.Monad.Trans.Resource
import  Control.Monad.Trans.Reader

import Data.Time
import Data.Word
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Read as T

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.CSV.Conduit

import qualified Database.MongoDB as Mongo

parseHackageDate :: T.Text -> Day
parseHackageDate t0 = either (const $ error "cannot parse Hackage Date") id $ do
    (y, t1) <- T.decimal t0
    (m, t2) <- T.decimal (T.tail t1)
    (d, _)  <- T.decimal (T.tail t2)
    return $ fromGregorian y m d

combineVersions :: Monad m => ConduitM [T.Text] (T.Text,Day,Int) m ()
combineVersions = awaitForever $ \(p:d:_:c:_) ->
    go p (parseHackageDate d) (read $ T.unpack c)
  where
    go p d = loop
      where
        loop !c = await >>= \case
            Just new@(p':dt:_:c':_) -> let d' = parseHackageDate dt in
                if p == p' && d == d'
                then loop (c + read (T.unpack c'))
                else yield (p, d, c) >> leftover new
            _ -> return ()

toDocument :: Mongo.ObjectId -> (T.Text,Day,Int) -> Mongo.Document
toDocument oid (_, day, cnt) =
    [ "package" Mongo.=: oid
    , "date"    Mongo.=: Mongo.UTC (UTCTime day 0)
    , "count"   Mongo.=: cnt
    ]

lastUpdate :: Mongo.Select q => q
lastUpdate = (Mongo.select ["key" Mongo.=: Mongo.String "last_update_downloads"] "config")

chunking :: Monad m => Int -> Conduit a m [a]
chunking n = loop
  where
    loop = do
        c <- CL.take n
        unless (null c) $ yield c >> loop

main :: IO ()
main = do
    file:_ <- getArgs
    hostName:mongoPort:db:user:passwd:_ <- fmap lines . liftIO $ readFile "config"

    pipe <- Mongo.connect' 6 (Mongo.Host hostName $ Mongo.PortNumber (fromIntegral (read mongoPort :: Word16)))
    Mongo.access pipe Mongo.master (T.pack db) $ do
        _ <- Mongo.auth (T.pack user) (T.pack passwd)
        mongoEnv <- ask
        since <- maybe (ModifiedJulianDay 0) (utctDay . Mongo.at "value") <$>
            Mongo.findOne lastUpdate

        cur <- Mongo.find (Mongo.select [] "cabal") { Mongo.project = map (Mongo.=: Mongo.Int64 1) ["_id", "package"] }

        let loop m = Mongo.next cur >>= \case
                Nothing -> return m
                Just a  -> loop $ H.insert (Mongo.at "package" a) (Mongo.at "_id" a) m

        pidMap <- loop (H.empty :: H.HashMap T.Text Mongo.ObjectId)

        lastDate <- runResourceT $ CB.sourceFile file
            =$ intoCSV defCSVSettings
            =$ combineVersions
            =$ CL.filter (\(_,d,_) -> since < d)
            =$ CL.mapMaybe (\t@(p,_,_) -> (,t) <$> H.lookup p pidMap)
            =$ chunking 1000
            $$ CL.foldM (\ld ds -> flip runReaderT mongoEnv $ do
                liftIO $ hPutChar stderr '.' >> hFlush stderr
                Mongo.insertAll_ "downloads" (map (uncurry toDocument) ds)
                return (maximum $ ld : map (\(_,(_,dy,_)) -> dy) ds)
                ) (ModifiedJulianDay 0)

        when (lastDate /= ModifiedJulianDay 0) $ 
            Mongo.upsert lastUpdate [ "key"   Mongo.=: Mongo.String "last_update_downloads"
                                    , "value" Mongo.=: Mongo.UTC (UTCTime lastDate 0)
                                    ]

    Mongo.close pipe
