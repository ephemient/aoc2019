{-|
Module:         Day18
Description:    <https://adventofcode.com/2019/day/18 Day 18: Many-Worlds Interpretation>
-}
{-# LANGUAGE FlexibleContexts, TupleSections, TypeApplications, ViewPatterns #-}
module Day18 (day18a, day18b) where

import Control.Applicative (liftA2)
import Control.Arrow ((***))
import Control.Monad ((>=>), filterM, guard)
import Control.Monad.State (evalStateT, gets, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (execWriter, tell)
import Data.Char (isLower, isUpper, toLower)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Functor (($>))
import Data.Heap (FstMinPolicy, Heap, HeapItem)
import qualified Data.Heap as Heap (insert, singleton, view)
import Data.List hiding ((\\))
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.Map.Lazy (Map, (!?))
import qualified Data.Map.Lazy as Map (filter, fromList, insert, keys, mapMaybe)
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Data.Set (Set, (\\))
import qualified Data.Set as Set (empty, fromList, insert, member, null, singleton, size, union, unions)

data Item = Open | Key Char | Door Char

bfsWithM :: (HeapItem pol item, Traversable t, Monad m) =>
    (item -> m (t item)) -> Heap pol item -> m ()
bfsWithM f = bfsWithM' where
    bfsWithM' (Heap.view -> Just (item, queue)) = f item >>=
        bfsWithM' . foldl' (flip Heap.insert) queue
    bfsWithM' _ = return ()

bfsM :: (Ord k, Monad m) => (a -> k) -> (Int -> a -> m [a]) -> a -> m ()
bfsM proj f start = flip evalStateT (Set.singleton $ proj start) .
    bfsWithM (g >=> filterM (checkState . proj . snd)) $
    Heap.singleton @FstMinPolicy (0, start) where
    g (d, a) = fmap (d + 1,) <$> lift (f d a)
    checkState s = gets (not . Set.member s) <* modify (Set.insert s)

neighbors :: (Num a) => (a, a) -> [(a, a)]
neighbors (x, y) = [(x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)]

reachableKeys :: (Num a, Ord a) => Map (a, a) Item -> (a, a) -> Set Char
reachableKeys maze = execWriter . bfsM id go where
    go _ pos = sequence
      [ case item of
            Key c -> tell (Set.singleton c) $> pos'
            _ -> return pos'
      | pos' <- neighbors pos
      , item <- maybeToList $ maze !? pos'
      ]

explore :: (Num a, Ord a) =>
    Map (a, a) Item -> (a, a) -> [[((Char, Int), Set Char)]]
explore maze pos0 = execWriter $ bfsM proj go
    (pos0, Set.size $ reachableKeys maze pos0, Set.empty, Set.empty, []) where
    proj (pos, _, doors, _, path) = (pos, doors, fst . fst <$> path)
    go _ (_, 0, _, _, path) = tell [reverse path] $> []
    go d (pos, pending, doors, keys, path) = return
      [ (pos', pending', doors', keys', path')
      | pos' <- neighbors pos
      , item <- maybeToList $ maze !? pos'
      , (pending', doors', keys', path') <- case item of
            Key c
              | Set.member c doors -> []
              | not $ Set.member c keys ->
                [ ( pending - 1
                  , doors
                  , Set.insert c keys
                  , ((c, d + 1), doors) : path
                  )
                ]
            Door c | not $ Set.member c keys ->
                [(pending, Set.insert c doors, keys, path)]
            _ -> [(pending, doors, keys, path)]
      ]

parse :: String -> Map (Int, Int) Char
parse input = Map.fromList
  [ ((x, y), char)
  | (y, line) <- zip [0..] $ lines input
  , (x, char) <- zip [0..] line
  ]

parseItem :: Char -> Maybe Item
parseItem '.' = Just Open
parseItem '@' = Just Open
parseItem c | isLower c = Just $ Key c
parseItem c | isUpper c = Just . Door $ toLower c
parseItem _ = Nothing

day18a :: String -> Maybe Int
day18a input = listToMaybe . mapMaybe complete $ explore maze pos0 where
    raw = parse input
    [pos0] = Map.keys $ Map.filter ('@' ==) raw
    maze = Map.mapMaybe parseItem raw
    complete path@(_:_)
      | all (Set.null . snd) path = Just . snd . fst $ last path
    complete _ = Nothing

day18b :: String -> Maybe Int
day18b input = fmap minimum . nonEmpty .
    mapMaybe (joinPaths Set.empty . map (maybe (Left 0) Right . nonEmpty)) .
    sequence $ fmap simplify . explore maze <$> botPositions where
    raw = parse input
    [(x, y)] = Map.keys $ Map.filter ('@' ==) raw
    botPositions = liftA2 (,) [x - 1, x + 1] [y - 1, y + 1]
    maze = Map.mapMaybe parseItem . foldr (uncurry Map.insert) raw $
        map (, '@') botPositions ++
        map (, '#') (liftA2 (,) [x - 1..x + 1] [y - 1..y + 1])
    simplify = map ((((Set.fromList *** maximum) . unzip) *** head) . unzip) .
        groupBy ((==) `on` snd)
    joinPaths _ (partitionEithers -> (ds, [])) = Just $ sum ds
    joinPaths keys paths = do
        let (Set.unions -> keys', paths') = unzip $ expend keys <$> paths
        guard . not $ Set.null keys'
        joinPaths (Set.union keys keys') paths'
    expend keys (Right (((keys', d), doors) :| path))
      | Set.null $ doors \\ keys = (keys', path') where
        path' = maybe (Left d) (Right . fmap (fmap (\\ keys))) $ nonEmpty path
    expend keys path = (Set.empty, fmap (fmap $ fmap (\\ keys)) path)
