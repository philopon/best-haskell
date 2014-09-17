{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import System.Environment

import Control.Monad.Trans
import Control.Monad.Trans.Resource

import qualified Data.Version as D
import qualified Text.ParserCombinators.ReadP as D
import qualified Distribution.Text as D
import qualified Distribution.Package as D
import qualified Distribution.PackageDescription as D
import qualified Distribution.PackageDescription.Parse as D

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.List
import Data.Function

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Zlib (ungzip)

import qualified Data.Text as T

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

import Tar

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

gpdToJSON :: D.GenericPackageDescription -> Pair
gpdToJSON gpd = 
    let pd = D.packageDescription gpd
    in (T.pack . unPkgName . D.pkgName . D.package) pd .= object
        [ "version"     .= (D.display . D.pkgVersion . D.package) pd
        , "license"     .= (D.display . D.license) pd
        , "copyright"   .= D.copyright pd
        , "maintainer"  .= D.maintainer pd
        , "author"      .= D.author pd
        , "stability"   .= D.stability pd
        , "homepage"    .= D.homepage pd
        , "packageUrl"  .= D.pkgUrl pd
        , "bugReports"  .= D.bugReports pd
        , "synopsis"    .= D.synopsis pd
        , "description" .= D.description pd
        , "category"    .= (filter (not . T.null) . map T.strip . T.splitOn "," . T.pack . D.category) pd
        , "executables" .= (map fst . D.condExecutables) gpd
        , "hasLibrary"  .= (isJust . D.condLibrary) gpd
        ]
  where
    unPkgName (D.PackageName  n) = n

main :: IO ()
main = runResourceT $ do
    f:_ <- liftIO getArgs
    l <- packageConduit f $$ CL.map gpdToJSON =$ CL.consume
    liftIO . L8.putStrLn $ encode (object l)
