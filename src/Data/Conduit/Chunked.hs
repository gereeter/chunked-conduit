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
import Data.Conduit.List (sinkNull, peek, map)

import Prelude hiding (map)

---------------------- Primitive chunking ------------------------------

data Chunked a = InChunk a | EndOfChunk

makeChunk :: Monad m => Conduit a m (Chunked a)
makeChunk = map InChunk >> yield EndOfChunk

takeChunk :: Monad m => Conduit (Chunked a) m a
takeChunk = do
    mval <- await
    case mval of
        Nothing            -> return ()
        Just (InChunk val) -> yield val >> takeChunk
        Just EndOfChunk    -> return ()

------------------- Composite Chunking ---------------------------------

numChunks :: Monad m => Consumer (Chunked a) m Int
numChunks = accumChunks (\x -> return (x + 1)) 0

concatChunks :: Monad m => Conduit (Chunked a) m a
concatChunks = foreverEOF takeChunk

mapChunks :: Monad m => Conduit i m o -> Conduit (Chunked i) m (Chunked o)
mapChunks p = foreverEOF (takeChunk +$= p =$= makeChunk)

foldChunks :: Monad m => Consumer i m r -> Conduit (Chunked i) m r
foldChunks p = foreverEOF ((takeChunk +$= p) >>= yield)

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
