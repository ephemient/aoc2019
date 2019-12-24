{-|
Module:         Day18
Description:    <https://adventofcode.com/2019/day/18 Day 18: Many-Worlds Interpretation>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module Day18 (day18a, day18b) where

import Common (bfsM, dijkstraM, neighbors)
import Control.Monad (filterM, guard, when)
import Control.Monad.Cont (callCC, runCont)
import Control.Monad.State (evalStateT, gets, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (execWriter, tell)
import Data.Char (isLower, isUpper, toLower)
import Data.Either (isRight)
import Data.Functor (($>))
import Data.Heap (FstMinPolicy)
import qualified Data.Heap as Heap (singleton)
import Data.List hiding ((\\))
import Data.Map.Lazy (Map, (!?))
import qualified Data.Map.Lazy as Map (assocs, delete, empty, filter, fromList, insert, keys, lookup, mapMaybe, singleton, union, size)
import Data.Maybe (maybeToList)
import Data.Set (Set, (\\))
import qualified Data.Set as Set (empty, foldr, fromList, insert, null, size)

mazePaths :: (Num a, Ord a, Ord b, Ord c) =>
    (c -> Bool) -> Map (a, a) (Maybe (Either b b)) -> Map (a, a) c -> Map c (Map c (Set b, Int))
mazePaths isTerminal maze locs = Map.fromList
  [ (c0, execWriter $ bfsM fst (go c0) (pos0, Set.empty))
  | (pos0, c0) <- Map.assocs locs
  ] where
    go c0 d (pos, doors)
      | Just c1 <- locs !? pos, c0 /= c1, isTerminal c1
      = tell (Map.singleton c1 (doors, d)) $> []
      | otherwise = return
      [ (pos', doors')
      | pos' <- neighbors pos
      , item <- maybeToList $ maze !? pos'
      , let doors' = maybe id (either Set.insert $ flip const) item doors
      ]

day18 :: (Num a, Ord a, Ord b) =>
    Map (a, a) (Maybe (Either b b)) -> [(a, a)] -> Maybe Int
day18 maze start = flip runCont id $ callCC $ \exit ->
    flip evalStateT Map.empty $ dijkstraM (go $ lift . exit . Just)
        (Heap.singleton @FstMinPolicy
            (0, (Left . fst <$> zip [0..] start, Set.empty))) $> Nothing where
    startLocs = Map.fromList . zip start $ Left <$> [0 :: Int ..]
    keyLocs = Map.mapMaybe (>>= either (const Nothing) (Just . Right)) maze
    paths = mazePaths isRight maze $ Map.union startLocs keyLocs
    go exit (d, (_, keys)) | Set.size keys == Map.size keyLocs = exit d
    go _ (d, state@(poss, keys)) = do
        seen <- gets $ maybe False (<= Left d) . Map.lookup state
        if seen then return [] else modify (Map.insert state $ Left d) >>
            let new (d', state') = do
                    ok <- gets $ maybe True (> Right d') . Map.lookup state'
                    when ok (modify . Map.insert state' $ Right d') $> ok
            in filterM new $ do
            (pre, cur : post) <- zip (inits poss) (tails poss)
            (Right key, (doors, w)) <- maybeToList (paths !? cur) >>= Map.assocs
            guard . Set.null $ doors \\ keys
            return (d + w, (pre ++ Right key : post, Set.insert key keys))

parse :: String -> Map (Int, Int) Char
parse input = Map.fromList
  [ ((x, y), char)
  | (y, line) <- zip [0..] $ lines input
  , (x, char) <- zip [0..] line
  ]

parseItem :: Char -> Maybe (Maybe (Either Char Char))
parseItem '.' = Just Nothing
parseItem '@' = Just Nothing
parseItem c | isLower c = Just . Just $ Right c
parseItem c | isUpper c = Just . Just . Left $ toLower c
parseItem _ = Nothing

day18a :: String -> Maybe Int
day18a input = day18 maze start where
    raw = parse input
    start = Map.keys $ Map.filter ('@' ==) raw
    maze = Map.mapMaybe parseItem raw

day18b :: String -> Maybe Int
day18b input = day18 maze start where
    raw = parse input
    [(x, y)] = Map.keys $ Map.filter ('@' ==) raw
    center = (,) <$> [x - 1..x + 1] <*> [y - 1..y + 1]
    start = (,) <$> [x - 1, x + 1] <*> [y - 1, y + 1]
    maze = Set.foldr Map.delete (Map.mapMaybe parseItem raw) $
        Set.fromList center \\ Set.fromList start
