module Pmap
( pmap
) where

import Config (do_parallel, chunk_size)
import Control.Parallel.Strategies (withStrategy, parListChunk, rseq)

--------------------------------------------------------------------------------

pmap :: (a -> b) -> [a] -> [b]
pmap = if do_parallel then parallel_map else map

--------------------------------------------------------------------------------

parallel_map :: (a -> b) -> [a] -> [b]
parallel_map = parallel_map' chunk_size

parallel_map' :: Int -> (a -> b) -> [a] -> [b]
parallel_map' chunk f = withStrategy (parListChunk chunk rseq) . map f

--------------------------------------------------------------------------------
