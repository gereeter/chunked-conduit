module Data.Conduit.Chunked.Text (
      take
    , chunksOf
    , break
    , splitOn
    , lines
) where

import Data.Conduit
import Data.Conduit.List (sinkNull)
import Data.Conduit.Chunked.Base
import Data.Conduit.Chunked.Helper

import qualified Data.Text as T

import Prelude hiding (take, break, lines)

take :: Monad m => Int -> Conduit T.Text m T.Text
take n = do
    mval <- await
    case mval of
        Nothing -> return ()
        Just val -> case T.compareLength val n of
            LT -> yield val >> take (n - T.length val)
            EQ -> yield val
            GT -> case T.splitAt n val of
                (left, right) -> leftover right >> yield left

chunksOf :: Monad m => Int -> Conduit T.Text m (Chunked T.Text)
chunksOf m = foreverEOF (take m =$= makeChunk)

break :: Monad m => (Char -> Bool) -> Conduit T.Text m T.Text
break p = do
    mval <- await
    case mval of
        Nothing -> return ()
        Just val -> case T.break p val of
            (left, right) | T.null right -> yield left >> break p
                          | otherwise    -> leftover right >> yield left

splitOn :: Monad m => (Char -> Bool) -> Conduit T.Text m (Chunked T.Text)
splitOn p = foreverEOF ((break p >> (take 1 =$= sinkNull)) =$= makeChunk)

lines :: Monad m => Conduit T.Text m (Chunked T.Text)
lines = splitOn (== '\n')
