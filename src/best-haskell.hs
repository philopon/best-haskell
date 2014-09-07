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
import qualified Web.Apiary.MongoDB as M
import Network.Wai.Handler.Warp

import Control.Monad.Trans.Control
import Control.Applicative
import Control.Exception

import qualified Data.Aeson as A
import Data.Word
import Data.Maybe
import Data.Char
import Data.Typeable
import Data.Time
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as S8

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

serv :: (Extensions '[M.MongoDB] -> IO Application) -> IO ()
serv app = do
    port:_ <- getArgs
    (u,w,h,p,d) <- getMongoDBConfig mongoEnvName
    let mongoConf = def { M.mongoDBHost     = M.Host h (M.PortNumber $ fromIntegral p)
                        , M.mongoDBAuth     = Just (u, w)
                        , M.mongoDBDatabase = d
                        , M.mongoDBTimeout  = 30
                        }
    serverWith (M.initMongoDB mongoConf) (run $ read port) app

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

rankingAction :: (MonadIO m, MonadBaseControl IO m, Has M.MongoDB exts)
              => Word -> Maybe Day -> [T.Text] -> Maybe CabalType -> ActionT exts m ()
rankingAction limit mbsince cats typ = do
    doc <- M.access . M.aggregate "packages" $ aggrQuery limit mbsince cats typ
    lazyBytes . A.encode $ map aggrToValue doc

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
        ["$project" M.=: [ "downloads" M.=: M.Int64 1
                         , "synopsis"  M.=: M.Int64 1
                         , "author"    M.=: M.Int64 1
                         , "name"      M.=: M.Int64 1
                         ]] :
        [ "$match"  M.=: ["downloads.date" M.=: ["$gt" M.=: M.UTC (UTCTime since 0)]]] :
        [ "$unwind" M.=: ("$downloads" :: T.Text)] :
        ["$group"   M.=: [ "_id"      M.=: ("$name" :: T.Text)
                         , "name"     M.=: ["$first" M.=: ("$name"     :: T.Text)]
                         , "synopsis" M.=: ["$first" M.=: ("$synopsis" :: T.Text)]
                         , "author"   M.=: ["$first" M.=: ("$author"   :: T.Text)]
                         , "total"    M.=: ["$sum"   M.=: ("$downloads.count" :: T.Text)]
                         ]] :
        ["$sort"    M.=: ["total" M.=: (-1 :: Int)]] :
        ["$limit"   M.=: M.Int64 (fromIntegral limit)] : []
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
