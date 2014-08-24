{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common (getMongoConfig) where

import System.Environment
import System.Process
import System.Exit

import Control.Applicative
import Control.Exception

import qualified Data.Text as T
import qualified Data.Text.IO as T

getMongoConfig :: IO (T.Text, T.Text, String, Int, T.Text)
getMongoConfig = getMongoConfig' "MONGOHQ_URL"

getMongoConfig' :: String -> IO (T.Text, T.Text, String, Int, T.Text)
getMongoConfig' ev = do
    s0 <- handle (\(_::SomeException) -> getHerokuConfig ev) $ T.pack <$> getEnv ev
    let (_,      s1)    = T.breakOnEnd "://" s0
        (user,   s2) = T.break (== ':') s1
        (passwd, s3) = T.break (== '@') (T.tail s2)
        (host,   s4) = T.break (== ':') (T.tail s3)
        (port,   s5) = T.break (== '/') (T.tail s4)
    return (user, passwd, T.unpack host, read $ T.unpack port, T.tail s5)

getHerokuConfig :: String -> IO T.Text
getHerokuConfig key = do
    (_, Just stdout, _, h) <- createProcess
        (proc "heroku" ["config:get", key]) {std_out = CreatePipe}
    xc <- waitForProcess h
    if xc == ExitSuccess
        then T.hGetLine stdout
        else fail "heroku command failure."
