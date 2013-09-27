{-# LANGUAGE RankNTypes #-}

module Data.Conduit.Chunked (
      Chunked
    , singletonChunk
    , headChunk
    , numChunks
    , concatChunks
    , mapChunks
    , foldChunks
    , accumChunks
) where

import Data.Conduit
import Data.Conduit.List (sinkNull, peek, map)

import Prelude hiding (map)

------------------- General Combinators ------------------------------

(+$=) :: Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
left +$= right = left =$= right' where
    right' = do
        ret <- right
        sinkNull
        return ret

foreverEOF :: Monad m => ConduitM i o m () -> ConduitM i o m ()
foreverEOF p = peek >>= maybe (return ()) (const (p >> foreverEOF p))

---------------------- General chunking --------------------------------

data Chunked a = InChunk a | EndOfChunk deriving (Show, Eq)

singletonChunk :: Monad m => Conduit a m (Chunked a)
singletonChunk = map InChunk >> yield EndOfChunk

headChunk :: Monad m => Conduit (Chunked a) m a
headChunk = do
    mval <- await
    case mval of
        Nothing            -> return ()
        Just (InChunk val) -> yield val >> headChunk
        Just EndOfChunk    -> return ()

numChunks :: Monad m => Consumer (Chunked a) m Int
numChunks = accumChunks (\x -> return (x + 1)) 0

concatChunks :: Monad m => Conduit (Chunked a) m a
concatChunks = foreverEOF headChunk

mapChunks :: Monad m => Conduit i m o -> Conduit (Chunked i) m (Chunked o)
mapChunks p = foreverEOF (headChunk +$= p =$= singletonChunk)

foldChunks :: Monad m => Consumer i m r -> Conduit (Chunked i) m r
foldChunks p = foreverEOF ((headChunk +$= p) >>= yield)

accumChunks :: Monad m => (a -> ConduitM i o m a) -> a -> ConduitM (Chunked i) o m a
accumChunks f = start where
    start a = peek >>= maybe (return a) (const $ loop (f a))
    loop consumer = (headChunk +$= consumer) >>= start
