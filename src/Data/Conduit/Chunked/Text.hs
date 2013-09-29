module Data.Conduit.Chunked.Text (
      take
    , chunksOf
    , takeWhile
    , splitOn
    , lines
    , anyChar
    , groupBy
) where

import Data.Conduit
import Data.Conduit.List (sinkNull, filter)
import Data.Conduit.Chunked.Base
import Data.Conduit.Chunked.Helper

import qualified Data.Text as T

import Prelude hiding (take, takeWhile, lines, filter)

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

takeWhile :: Monad m => (Char -> Bool) -> Conduit T.Text m T.Text
takeWhile p = do
    mval <- await
    case mval of
        Nothing -> return ()
        Just val -> case T.span p val of
            (left, right) | T.null right -> yield left >> takeWhile p
                          | otherwise    -> leftover right >> yield left

splitOn :: Monad m => (Char -> Bool) -> Conduit T.Text m (Chunked T.Text)
splitOn p = foreverEOF ((takeWhile (not . p) >> (take 1 =$= sinkNull)) =$= makeChunk)

lines :: Monad m => Conduit T.Text m (Chunked T.Text)
lines = splitOn (== '\n')

anyChar :: Monad m => Consumer T.Text m (Maybe Char)
anyChar = do
    mval <- await
    case mval of
        Nothing -> return Nothing
        Just val | T.null val -> anyChar
                 | otherwise  -> leftover (T.tail val) >> return (Just (T.head val))

groupBy :: Monad m => (Char -> Char -> Bool) -> Conduit T.Text m (Chunked T.Text)
groupBy eq = filter (not . T.null) =$= foreverEOF ((anyChar >>= maybe (return ()) (takeWhile . eq)) =$= makeChunk)
