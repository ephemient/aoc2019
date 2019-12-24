{-# LANGUAGE FlexibleContexts, TupleSections, TypeApplications, ViewPatterns #-}
module Common (bfsM, dijkstraM, dijkstraM2, egcd, findCycle, neighbors) where

import Control.Monad ((>=>), filterM)
import Control.Monad.State (evalStateT, gets, modify)
import Control.Monad.Trans (lift)
import Data.Heap (FstMinPolicy, Heap, HeapItem)
import qualified Data.Heap as Heap (insert, singleton, view)
import Data.List (find, foldl', scanl')
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map (alter, empty, insertLookupWithKey, insert, insertWith, minViewWithKey)
import qualified Data.Set as Set (delete, empty, insert, member, minView, null, singleton, union)

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

dijkstraM :: (HeapItem pol item, Foldable t, Monad m) =>
    (item -> m (t item)) -> Heap pol item -> m ()
dijkstraM f = dijkstraM' where
    dijkstraM' (Heap.view -> Just (item, queue)) = f item >>=
        dijkstraM' . foldl' (flip Heap.insert) queue
    dijkstraM' _ = return ()

dijkstraM2 :: (Ord d, Ord k, Foldable t, Foldable t1, Monad m) =>
    (a -> k) -> (d -> a -> m (t (d, a))) -> t1 (d, a) -> m ()
dijkstraM2 proj f = dijkstraM2' . foldl' build (Map.empty, Map.empty) where
    dijkstraM2' (Map.minViewWithKey -> Just ((prio, keys), queue), items) =
        f prio' item >>= dijkstraM2' . foldl' build (queue', items) where
        Just (((items !) -> (item, prio')), keys') = Set.minView keys
        queue' = if Set.null keys' then queue else Map.insert prio keys' queue
    dijkstraM2' _ = return ()
    build (queue, items) (prio, item@(proj -> key)) = (queue', items') where
        (prev, items') = insertReplacing key (item, prio) items
        queue' = case prev of
            Just (_, prio') -> if prio' <= prio then queue else
                insertUnion prio key $ alterDelete prio' key queue
            Nothing -> insertUnion prio key queue
    insertReplacing = Map.insertLookupWithKey $ const minSnd
    insertUnion key = Map.insertWith Set.union key . Set.singleton
    alterDelete key value = Map.alter (>>= deleteOrNull value) key
    deleteOrNull value set = if Set.null set' then Nothing else Just set' where
        set' = Set.delete value set
    minSnd x@(_, px) y@(_, py) = if px <= py then x else y

bfsM :: (Ord k, Monad m) => (a -> k) -> (Int -> a -> m [a]) -> a -> m ()
bfsM proj f start = flip evalStateT (Set.singleton $ proj start) .
    dijkstraM (g >=> filterM (checkState . proj . snd)) $
    Heap.singleton @FstMinPolicy (0, start) where
    g (d, a) = fmap (d + 1,) <$> lift (f d a)
    checkState s = gets (not . Set.member s) <* modify (Set.insert s)
