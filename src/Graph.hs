{-# LANGUAGE FlexibleContexts, TupleSections, TypeApplications, ViewPatterns #-}
module Graph (bfsM, dijkstraM) where

import Control.Monad ((>=>), filterM)
import Control.Monad.State (evalStateT, gets, modify)
import Control.Monad.Trans (lift)
import Data.Heap (FstMinPolicy, Heap, HeapItem)
import qualified Data.Heap as Heap (insert, singleton, view)
import Data.List (foldl')
import qualified Data.Set as Set (insert, member, singleton)

dijkstraM :: (HeapItem pol item, Traversable t, Monad m) =>
    (item -> m (t item)) -> Heap pol item -> m ()
dijkstraM f = dijkstraM' where
    dijkstraM' (Heap.view -> Just (item, queue)) = f item >>=
        dijkstraM' . foldl' (flip Heap.insert) queue
    dijkstraM' _ = return ()

bfsM :: (Ord k, Monad m) => (a -> k) -> (Int -> a -> m [a]) -> a -> m ()
bfsM proj f start = flip evalStateT (Set.singleton $ proj start) .
    dijkstraM (g >=> filterM (checkState . proj . snd)) $
    Heap.singleton @FstMinPolicy (0, start) where
    g (d, a) = fmap (d + 1,) <$> lift (f d a)
    checkState s = gets (not . Set.member s) <* modify (Set.insert s)
