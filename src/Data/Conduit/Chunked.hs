{-# LANGUAGE RankNTypes #-}

module Data.Conduit.Chunked (
      Chunked
    , makeChunk
    , takeChunk
    , numChunks
    , concatChunks
    , mapChunks
    , foldChunks
    , accumChunks
) where

import Data.Conduit
import Data.Conduit.List (sinkNull, peek)
import Data.Conduit.Chunked.Base

-- | Counts the total number of chunks in a stream.
numChunks :: Monad m => Consumer (Chunked a) m Int
numChunks = accumChunks (\x -> return (x + 1)) 0

-- | Unpacks all the chunks in a stream, yielding all the values in all
-- the chunks.
concatChunks :: Monad m => Conduit (Chunked a) m a
concatChunks = foreverEOF takeChunk

-- | Runs a given conduit repeatedly on every chunk that comes in.
mapChunks :: Monad m => Conduit i m o -> Conduit (Chunked i) m (Chunked o)
mapChunks p = foreverEOF (takeChunk +$= p =$= makeChunk)

-- | Runs a given consumer repeatedly on every chunk, streaming out the
-- return values.
foldChunks :: Monad m => Consumer i m r -> Conduit (Chunked i) m r
foldChunks p = foreverEOF ((takeChunk +$= p) >>= yield)

-- | Runs a given action on every input chunk, threading the return value
-- on the a chunk to the next action on the next chunk.
accumChunks :: Monad m => (a -> ConduitM i o m a) -> a -> ConduitM (Chunked i) o m a
accumChunks f = start where
    start a = peek >>= maybe (return a) (const $ loop (f a))
    loop consumer = (takeChunk +$= consumer) >>= start

---------------------- Helper Functions --------------------------------

(+$=) :: Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
left +$= right = left =$= right' where
    right' = do
        ret <- right
        sinkNull
        return ret

foreverEOF :: Monad m => ConduitM i o m () -> ConduitM i o m ()
foreverEOF p = peek >>= maybe (return ()) (const (p >> foreverEOF p))
