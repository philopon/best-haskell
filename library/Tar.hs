{-# LANGUAGE NoMonomorphismRestriction #-}

module Tar (conduitUnTar, TypeFlag, TarHeader(..)) where

import Control.Monad

import Data.Conduit
import qualified Data.Conduit.Binary as CB

import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

conduitUnTar :: Monad m => Conduit S.ByteString m (TarHeader, L.ByteString)
conduitUnTar = do
    hB <- CB.take 512
    case runGetOrFail getTarHeader hB of
        Left (_,_,e)    -> do
            if L.all (== 0) hB
                then return ()
                else fail e
        Right (_,_,hdr) -> do
            let pad = let p = tarSize hdr `mod` 512 in if p == 0 then 0 else 512 - p
            bdy <- CB.take (tarSize hdr)
            CB.drop pad
            yield (hdr, bdy)
            conduitUnTar

newtype TypeFlag = TypeFlag Word8
    deriving Show

data TarHeader = TarHeader
    { tarName     :: S.ByteString
    , tarSize     :: Int
    , tarTypeFlag :: TypeFlag
    } deriving Show

getTarHeader :: Get TarHeader
getTarHeader = do
    name   <- getByteString 100
    c1     <- getByteString 24
    size   <- getByteString 12      -- 124
    c2     <- getByteString 12
    chksum <- getByteString 8       -- 148
    typflg <- getWord8              -- 156
    c3     <- getByteString 355     -- 157

    let sumBS = S.foldl' (\a w -> a + fromIntegral w) (0 :: Int)
        chk   = sum (map sumBS [name, c1, size, c2, c3]) + fromIntegral typflg + 256
        name' = S.takeWhile (/=0) name
        typflg' = readOctChar typflg
    
    guard (readOctNum chksum == chk)

    let size' =
            if typflg' == 0
                then readOctNum size
                else 0

    return $ TarHeader (S.takeWhile (/=0) name') size' (TypeFlag typflg')

readOctNum :: S.ByteString -> Int
readOctNum = S.foldl' (\a w -> a * 8 + (fromIntegral $ readOctChar w)) 0 . S.takeWhile (/=0)

readOctChar :: Word8 -> Word8
readOctChar w = w - 0x30
