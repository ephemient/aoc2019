{-# LANGUAGE FlexibleContexts, TupleSections, TypeApplications, ViewPatterns #-}
module Common (bfsM, dijkstraM, egcd, findCycle, neighbors) where

import Control.Monad ((>=>), filterM)
import Control.Monad.State (evalStateT, gets, modify)
import Control.Monad.Trans (lift)
import Data.Heap (FstMinPolicy, Heap, HeapItem)
import qualified Data.Heap as Heap (insert, singleton, view)
import Data.List (find, foldl', scanl')
import qualified Data.Set as Set (empty, insert, member, singleton)

-- |Extended GCD.
--
-- prop> gcd a b == (s, t, g) ==> a * s + b * t == g
egcd :: (Integral a) => a -> a -> (a, a, a)
egcd a 0 = (1, 0, a)
egcd a b = (t, s - q * t, abs g) where
    (q, r) = a `quotRem` b
    (s, t, g) = egcd b r

findCycle :: (Ord a) => [a] -> Maybe a
findCycle xs = fmap fst . find (uncurry Set.member) . zip xs $
    scanl' (flip Set.insert) Set.empty xs

neighbors :: (Num a) => (a, a) -> [(a, a)]
neighbors (x, y) = [(x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)]

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
