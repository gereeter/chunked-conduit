-- | A collection of general conduit helper functions that probably should
-- be in conduit, but aren't and are really useful for chunking
module Data.Conduit.Chunked.Helper where

import Data.Conduit
import Data.Conduit.List (sinkNull, peek)

(+$=) :: Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
left +$= right = left =$= right' where
    right' = do
        ret <- right
        sinkNull
        return ret

foreverEOF :: Monad m => ConduitM i o m () -> ConduitM i o m ()
foreverEOF p = peek >>= maybe (return ()) (const (p >> foreverEOF p))
