{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.IO
import System.Environment
import Network.HTTP.Client

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource

import Tar
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Zlib (ungzip)

import qualified Data.Text as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

import qualified Data.Version as D
import qualified Text.ParserCombinators.ReadP as D
import qualified Distribution.Text as D
import qualified Distribution.Package as D
import qualified Distribution.PackageDescription as D
import qualified Distribution.PackageDescription.Parse as D

import qualified Database.MongoDB as Mongo

import Common

parseOkOnly :: Monad m => Conduit (D.ParseResult a) m a
parseOkOnly = awaitForever $ \case
    D.ParseOk _ a -> yield a
    _ -> return ()

maxVersion :: [(TarHeader, b)] -> (TarHeader, b)
maxVersion = maximumBy
    (compare `on`
    (readV . S8.unpack . S8.takeWhile (/= '/') . S8.tail . S8.dropWhile (/= '/') . tarName . fst))
  where
    readV s = case D.readP_to_S D.parseVersion s of
        a -> case filter (null . snd) a of
            (v,_):_ -> v
            _       -> error "cannot read version."

packageConduit :: MonadResource m => FilePath -> Producer m D.GenericPackageDescription
packageConduit file =
    CB.sourceFile file
    =$ ungzip
    =$ conduitUnTar
    =$ CL.filter ((S.isSuffixOf ".cabal") . tarName . fst)
    =$ CL.groupBy ((==) `on` (S8.takeWhile (/= '/') . tarName . fst))
    =$ CL.map (D.parsePackageDescription . L8.unpack . snd . maxVersion)
    =$ parseOkOnly

chunking :: Monad m => Int -> Conduit a m [a]
chunking n = loop
  where
    loop = do
        c <- CL.take n
        unless (null c) $ yield c >> loop

addMembers :: (MonadThrow m, MonadIO m) => Bool -> Manager
           -> Conduit D.GenericPackageDescription m ([T.Text], D.GenericPackageDescription)
addMembers logging mgr = awaitForever $ \pkg -> do
    req <- parseUrl $ "http://hackage.haskell.org/package/" ++ packageName pkg ++ "/maintainers/"
    res <- liftIO $ httpLbs req {requestHeaders = ("Accept", "application/json") : requestHeaders req} mgr
    when logging $ liftIO $ hPutChar stderr '.' >> hFlush stderr
    maybe (return ()) (\a -> yield (a, pkg)) $ JSON.decode (responseBody res) >>= members 
  where
    members = \case 
        JSON.Object o -> H.lookup "members" o >>= \case
            JSON.Array a -> forM (V.toList a) $ \case
                JSON.Object o' -> H.lookup "username" o' >>= \case
                    JSON.String s -> return s
                    _ -> Nothing
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing

main :: IO ()
main = withManager defaultManagerSettings $ \mgr -> do
    file:_ <- liftIO getArgs
    (user,passwd,hostName,mongoPort,db) <- getMongoConfig

    pipe <- Mongo.connect' 6 (Mongo.Host hostName $ Mongo.PortNumber (fromIntegral mongoPort))

    Mongo.access pipe Mongo.master db $ do
        _ <- Mongo.auth user passwd
        n <- Mongo.count $ Mongo.select [] "cabal"
        mongoEnv <- ask
        if n == 0
            then runResourceT $ packageConduit file =$ addMembers True mgr =$ chunking 100 $$ CL.mapM_
                (\pds -> flip runReaderT mongoEnv $ do
                    liftIO $ hPutChar stderr '*' >> hFlush stderr
                    Mongo.insertAll_ "cabal" (map (uncurry toDocument) pds))

            else do
                cur <- Mongo.find (Mongo.select [] "cabal") { Mongo.project =
                    ["package" Mongo.=: Mongo.Int64 1, "version" Mongo.=: Mongo.Int64 1]}
                let loop m = Mongo.next cur >>= \case
                        Nothing -> return m
                        Just a  -> loop $ H.insert (Mongo.at "package" a) (Mongo.at "version" a) m
                pv <- loop (H.empty :: H.HashMap T.Text T.Text)
                Mongo.closeCursor cur

                let filt gpd = case H.lookup (T.pack $ packageName gpd) pv of
                        Nothing -> True
                        Just v  -> v /= (T.pack $ packageVersion gpd)

                runResourceT $ packageConduit file =$ CL.filter filt =$ addMembers False mgr $$ CL.mapM_
                    (\(mbr, pd) -> flip runReaderT mongoEnv $ do
                        liftIO $ hPutChar stderr '.' >> hFlush stderr
                        let sel = Mongo.select ["package" Mongo.=: (Mongo.String . T.pack . packageName) pd] "cabal"
                        Mongo.upsert sel (toDocument mbr pd))

    Mongo.close pipe

packageName :: D.GenericPackageDescription -> String
packageName = unPkgName . D.pkgName . D.package . D.packageDescription
  where
    unPkgName (D.PackageName n) = n

packageVersion :: D.GenericPackageDescription -> String
packageVersion = D.display . D.pkgVersion . D.package . D.packageDescription

toDocument :: [T.Text] -> D.GenericPackageDescription -> Mongo.Document
toDocument ms gpd = let pd = D.packageDescription gpd in
    [ "package"     Mongo.:= (Mongo.String .T.pack . packageName) gpd
    , "version"     Mongo.:= (Mongo.String . T.pack . packageVersion) gpd

    , "copyright"   Mongo.:= (Mongo.String . T.pack . D.copyright) pd
    , "author"      Mongo.:= (Mongo.String . T.pack . D.author) pd
    , "maintainer"  Mongo.:= (Mongo.String . T.pack . D.maintainer) pd
    , "members"     Mongo.:= Mongo.Array (map Mongo.String ms)

    , "stability"   Mongo.:= (Mongo.String . T.pack . D.stability) pd

    , "homepage"    Mongo.:= (Mongo.String . T.pack . D.homepage) pd
    , "bugReports"  Mongo.:= (Mongo.String . T.pack . D.bugReports) pd

    , "license"     Mongo.:= (Mongo.String . T.pack . D.display . D.license) pd

    , "has_library" Mongo.:= (Mongo.val . isJust . D.condLibrary) gpd
    , "executables" Mongo.:= (Mongo.Array . map (Mongo.String . T.pack . fst) . D.condExecutables) gpd

    , "category"    Mongo.:= (categoryToValue . T.pack . D.category) pd
    , "synopsis"    Mongo.:= (Mongo.String . T.pack . D.synopsis) pd
    , "description" Mongo.:= (Mongo.String . T.pack . D.description) pd

    ] 
  where
    categoryToValue = Mongo.Array . map Mongo.String . filter (not . T.null) . map T.strip . T.splitOn ","
