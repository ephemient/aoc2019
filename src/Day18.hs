{-|
Module:         Day18
Description:    <https://adventofcode.com/2019/day/18 Day 18: Many-Worlds Interpretation>
-}
{-# LANGUAGE FlexibleContexts, GADTs, LambdaCase, NamedFieldPuns, NoMonomorphismRestriction, MultiWayIf, ParallelListComp, PatternGuards, RecordWildCards, ScopedTypeVariables, TransformListComp, TupleSections, TypeApplications, TypeFamilies, ViewPatterns #-}
module Day18 (day18a, day18b) where

import Control.Applicative (liftA2)
import Control.Monad ((>=>), filterM)
import Control.Monad.Cont (callCC, runCont)
import Control.Monad.State (evalStateT, gets, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (execWriter, tell)
import Data.Char (isLower, isUpper, toLower)
import Data.Either (partitionEithers)
import Data.Functor (($>))
import Data.Heap (FstMinPolicy, Heap, HeapItem(..))
import qualified Data.Heap as Heap (insert, singleton, toList, view)
import Data.List (foldl', inits, tails)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map ((!?), assocs, filter, fromList, insert, keys, keysSet, mapMaybe)
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set (empty, foldr', fromList, insert, member, null, size)

data Item = Open | Key { getId :: Char } | Door { getId :: Char } deriving (Show)

data Bot k = Bot
  { position :: (k, k)
  , visibleTargets :: Set (k, k)
  } deriving (Show)

data ExploreState k = ExploreState
  { steps :: k
  , bots :: [Bot k]
  , keys :: Set (k, k)
  , doors :: Set (k, k)
  , opened :: Set Char
  } deriving (Show)

bfsWithM :: (HeapItem pol item, Traversable t, Monad m) =>
    (item -> m (t item)) -> Heap pol item -> m ()
bfsWithM f = bfsWithM' where
    bfsWithM' (Heap.view -> Just (item, queue)) = f item >>=
        bfsWithM' . foldl' (flip Heap.insert) queue
    bfsWithM' _ = return ()

bfsM :: (HeapItem pol item, Ord k, Monad m) =>
    (item -> k) -> (item -> m [item]) -> Heap pol item -> m ()
bfsM proj f heap = flip evalStateT (Set.fromList $ proj <$> Heap.toList heap) $
    bfsWithM (lift . f >=> filterM (checkState . proj)) heap where
    checkState s = gets (not . Set.member s) <* modify (Set.insert s)

neighbors :: (Num a) => (a, a) -> [(a, a)]
neighbors (x, y) = [(x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)]

manhattan :: (Num a) => (a, a) -> (a, a) -> a
manhattan (x0, y0) (x1, y1) = abs (x1 - x0) + abs (y1 - y0)

makeBot :: (Num k, Ord k) =>
    Set (k, k) -> Set (k, k) -> Set (k, k) -> (k, k) -> Bot k
makeBot maze keys doors position = Bot {..} where
    visibleTargets = Set.fromList $ execWriter $ bfsM (snd . snd) go $
        Heap.singleton @FstMinPolicy (0, (0, position))
    go (_, (d, pos)) = catMaybes <$> sequence
      [ action (d + 1 + estimate, (d + 1, pos'))
      | pos' <- neighbors pos
      , pos' `Set.member` maze
      , let action
              | pos' `Set.member` keys = (tell [pos'] $>) . Just
              | pos' `Set.member` doors = (tell [pos'] $>) . const Nothing
              | otherwise = return . Just
            estimate = Set.foldr' (min . manhattan pos') 0 keys
      ]

newExploreState :: (Num k, Ord k) =>
    Map (k, k) Item -> k -> [(k, k)] -> Set Char -> ExploreState k
newExploreState maze steps botPositions opened = ExploreState {..} where
    bots = makeBot (Map.keysSet maze) keys doors <$> botPositions
    (Set.fromList -> keys, Set.fromList -> doors) =
        partitionEithers $ flip mapMaybe (Map.assocs maze) $ \case
            (pos, Key c) | not $ c `Set.member` opened -> Just $ Left pos
            (pos, Door c) | not $ c `Set.member` opened -> Just $ Right pos
            _ -> Nothing

explore :: (Num k, Ord k, Monad m) =>
    Map (k, k) Item -> (k -> m [((Int, k), ExploreState k)]) -> ((Int, k), ExploreState k) -> m [((Int, k), ExploreState k)]
explore _ exit (_, ExploreState {steps, keys}) | Set.null keys = exit steps
explore maze _ (_, es@ExploreState {bots}) = return
  [ ((Set.size $ opened es', steps es'), es')
  | (pre, bot@Bot {position = pos, ..} : post) <- inits bots `zip` tails bots
  , not $ Set.null visibleTargets
  , pos' <- neighbors pos
  , item <- maybeToList $ maze Map.!? pos'
  , not $ pos' `Set.member` doors es
  , let bots' = pre ++ bot {position = pos'} : post
        es' = if pos' `Set.member` keys es
            then newExploreState maze (steps es + 1) (position <$> bots') $
                getId item `Set.insert` opened es
            else es { steps = steps es + 1, bots = bots' }
  ]

parse :: (Enum a, Num a, Ord a) => String -> Map (a, a) Char
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

projectExploreState :: ExploreState k -> ([(k, k)], Set Char)
projectExploreState ExploreState {bots, opened} = (position <$> bots, opened)

day18a :: String -> Maybe Int
day18a input = flip runCont id $ callCC $ \exit ->
    bfsM (projectExploreState . snd) (explore maze $ exit . Just)
        (Heap.singleton @FstMinPolicy ((0, 0), es)) $> Nothing
  where
    raw = parse input
    botPositions@[_] = Map.keys $ Map.filter ('@' ==) raw
    maze = Map.mapMaybe parseItem raw
    es = newExploreState maze 0 botPositions Set.empty

day18b :: String -> Maybe Int
day18b input = flip runCont id $ callCC $ \exit ->
    bfsM (projectExploreState . snd) (explore maze $ exit . Just)
        (Heap.singleton @FstMinPolicy ((0, 0), es)) $> Nothing
  where
    raw = parse input
    [(x, y)] = Map.keys $ Map.filter ('@' ==) raw
    botPositions = liftA2 (,) [x - 1, x + 1] [y - 1, y + 1]
    maze = Map.mapMaybe parseItem . foldr (uncurry Map.insert) raw $
        map (, '@') botPositions ++
        map (, '#') (liftA2 (,) [x - 1..x + 1] [y - 1..y + 1])
    es = newExploreState maze 0 botPositions Set.empty
