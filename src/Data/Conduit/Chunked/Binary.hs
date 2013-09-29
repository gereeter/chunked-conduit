module Data.Conduit.Chunked.Binary (
      take
    , chunksOf
    , takeWhile
    , splitOn
    , anyWord8
    , groupBy
) where

import Data.Conduit
import Data.Conduit.List (sinkNull, filter)
import Data.Conduit.Chunked.Base
import Data.Conduit.Chunked.Helper

import Data.Word (Word8)

import qualified Data.ByteString as BS

import Prelude hiding (take, takeWhile, lines, filter)

take :: Monad m => Int -> Conduit BS.ByteString m BS.ByteString
take n = do
    mval <- await
    case mval of
        Nothing -> return ()
        Just val -> case compare (BS.length val) n of
            LT -> yield val >> take (n - BS.length val)
            EQ -> yield val
            GT -> case BS.splitAt n val of
                (left, right) -> leftover right >> yield left

chunksOf :: Monad m => Int -> Conduit BS.ByteString m (Chunked BS.ByteString)
chunksOf m = foreverEOF (take m =$= makeChunk)

takeWhile :: Monad m => (Word8 -> Bool) -> Conduit BS.ByteString m BS.ByteString
takeWhile p = do
    mval <- await
    case mval of
        Nothing -> return ()
        Just val -> case BS.span p val of
            (left, right) | BS.null right -> yield left >> takeWhile p
                          | otherwise    -> leftover right >> yield left

splitOn :: Monad m => (Word8 -> Bool) -> Conduit BS.ByteString m (Chunked BS.ByteString)
splitOn p = foreverEOF ((takeWhile (not . p) >> (take 1 =$= sinkNull)) =$= makeChunk)

anyWord8 :: Monad m => Consumer BS.ByteString m (Maybe Word8)
anyWord8 = do
    mval <- await
    case mval of
        Nothing -> return Nothing
        Just val | BS.null val -> anyWord8
                 | otherwise  -> leftover (BS.tail val) >> return (Just (BS.head val))

groupBy :: Monad m => (Word8 -> Word8 -> Bool) -> Conduit BS.ByteString m (Chunked BS.ByteString)
groupBy eq = filter (not . BS.null) =$= foreverEOF ((anyWord8 >>= maybe (return ()) (takeWhile . eq)) =$= makeChunk)
