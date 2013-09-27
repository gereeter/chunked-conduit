module Data.Conduit.Chunked.Base (
      Chunked
    , makeChunk
    , takeChunk
) where

import Data.Conduit
import Data.Conduit.List (map)

import Prelude hiding (map)

-- | An abstract type signaling that a stream of values is broken into
-- pieces. For example, in the same way that a @Producer a m ()@ represents
-- @m [a]@, a @Producer (Chunked a) m ()@ represents @m [[a]]@.
data Chunked a = InChunk a | EndOfChunk

-- | Takes in a stream of values and packages them together into a single
-- chunk. 'makeChunk' is analagous to @\x -> [x]@.
makeChunk :: Monad m => Conduit a m (Chunked a)
makeChunk = map InChunk >> yield EndOfChunk

-- | Takes in a stream of chunks, only consuming the first one, and streams
-- out the values inside that first chunk.
takeChunk :: Monad m => Conduit (Chunked a) m a
takeChunk = do
    mval <- await
    case mval of
        Nothing            -> return ()
        Just (InChunk val) -> yield val >> takeChunk
        Just EndOfChunk    -> return ()
