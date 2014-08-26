{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common (getMongoConfig, getMemcachedConfig, toAeson) where

import System.Environment
import System.Process
import System.Exit

import Control.Applicative
import Control.Exception

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import qualified Data.Bson  as B
import qualified Data.Aeson as A

getMemcachedConfig :: IO (T.Text, Int, T.Text, T.Text)
getMemcachedConfig = getMemcachedConfig' "MEMCACHIER"

getMemcachedConfig' :: String -> IO (T.Text, Int, T.Text, T.Text)
getMemcachedConfig' evpfx = do
    (s,p) <- T.break (== ':') <$> getHerokuConfig (evpfx ++ "_SERVERS")
    usr   <- getHerokuConfig (evpfx ++ "_USERNAME")
    pwd   <- getHerokuConfig (evpfx ++ "_PASSWORD")
    return (s, read $ T.unpack $ T.tail p, usr, pwd)


getMongoConfig :: IO (T.Text, T.Text, String, Int, T.Text)
getMongoConfig = getDatabaseConfig "MONGOHQ_URL"

getDatabaseConfig :: String -> IO (T.Text, T.Text, String, Int, T.Text)
getDatabaseConfig ev = do
    s0 <- handle (\(_::SomeException) -> getHerokuConfig ev) $ T.pack <$> getEnv ev
    let (_,      s1)    = T.breakOnEnd "://" s0
        (user,   s2) = T.break (== ':') s1
        (passwd, s3) = T.break (== '@') (T.tail s2)
        (host,   s4) = T.break (== ':') (T.tail s3)
        (port,   s5) = T.break (== '/') (T.tail s4)
    return (user, passwd, T.unpack host, read $ T.unpack port, (if T.null s5 then id else T.tail) s5)

getHerokuConfig :: String -> IO T.Text
getHerokuConfig key = do
    (_, Just stdout, _, h) <- createProcess
        (proc "heroku" ["config:get", key, "--app", "best-haskell"]) {std_out = CreatePipe}
    xc <- waitForProcess h
    if xc == ExitSuccess
        then T.hGetLine stdout
        else fail "heroku command failure."

toAeson :: B.Document -> A.Value
toAeson = A.object . map kv
  where
    kv (k B.:= v) = k A..= toAesonValue v

toAesonValue :: B.Value -> A.Value
toAesonValue (B.Float  b) = A.toJSON b
toAesonValue (B.String b) = A.toJSON b
toAesonValue (B.Array bs) = A.toJSON $ map toAesonValue bs
toAesonValue (B.Bool   b) = A.toJSON b
toAesonValue (B.UTC    b) = A.toJSON b
toAesonValue  B.Null      = A.Null
toAesonValue (B.Int32  b) = A.toJSON b
toAesonValue (B.Int64  b) = A.toJSON b
toAesonValue (B.ObjId   (B.Oid       u l)) = A.toJSON (u,l)
toAesonValue (B.Uuid    (B.UUID        b)) = A.toJSON $ T.decodeUtf8 b
toAesonValue (B.Bin     (B.Binary      b)) = A.toJSON $ T.decodeUtf8 b
toAesonValue (B.Md5     (B.MD5         b)) = A.toJSON $ T.decodeUtf8 b
toAesonValue (B.Fun     (B.Function    b)) = A.toJSON $ T.decodeUtf8 b
toAesonValue (B.UserDef (B.UserDefined b)) = A.toJSON $ T.decodeUtf8 b
toAesonValue (B.RegEx   (B.Regex     u l)) = A.toJSON (u,l)
toAesonValue (B.Sym     (B.Symbol      b)) = A.toJSON b
toAesonValue (B.Stamp   (B.MongoStamp  b)) = A.toJSON b
toAesonValue (B.MinMax   B.MinKey) = "Min"
toAesonValue (B.MinMax   B.MaxKey) = "Max"
toAesonValue (B.Doc b) = toAeson b
toAesonValue (B.JavaScr (B.Javascript d b)) = A.toJSON (toAeson d, b)
