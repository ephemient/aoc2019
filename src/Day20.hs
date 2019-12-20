{-|
Module:         Day20
Description:    <https://adventofcode.com/2019/day/20 Day 20: Donut Maze>
-}
{-# LANGUAGE TupleSections #-}
module Day20 (day20a, day20b) where

import Control.Monad (guard)
import Control.Monad.Cont (callCC, runCont)
import Data.Char (isAlpha)
import Data.Functor (($>))
import Data.List (tails, transpose)
import Data.Map.Lazy (Map, (!), (!?))
import qualified Data.Map.Lazy as Map (assocs, fromList, fromListWith)
import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, member, toList)
import Data.Tuple (swap)
import Graph (bfsM)

parse :: String -> (Set (Int, Int), Map (Int, Int) String)
parse input = (maze, portals) where
    maze = Set.fromList $ do
        (y, line) <- zip [0..] $ lines input
        (x, '.') <- zip [0..] line
        return (x, y)
    portals = Map.fromList $
      [ ((x + 2, y), [a, b])
      | (y, line) <- zip [0..] $ lines input
      , (x, a:b:'.':_) <- zip [0..] $ tails line
      , isAlpha a, isAlpha b
      ] ++
      [ ((x, y), [a, b])
      | (y, line) <- zip [0..] $ lines input
      , (x, '.':a:b:_) <- zip [0..] $ tails line
      , isAlpha a, isAlpha b
      ] ++
      [ ((x, y + 2), [a, b])
      | (x, column) <- zip [0..] $ transpose $ lines input
      , (y, a:b:'.':_) <- zip [0..] $ tails column
      , isAlpha a, isAlpha b
      ] ++
      [ ((x, y), [a, b])
      | (x, column) <- zip [0..] $ transpose $ lines input
      , (y, '.':a:b:_) <- zip [0..] $ tails column
      , isAlpha a, isAlpha b
      ]

invertMap :: (Ord a, Ord b) => Map a b -> Map b [a]
invertMap = Map.fromListWith (++) . map (fmap (:[]) . swap) . Map.assocs

neighbors :: (Num a) => (a, a) -> [(a, a)]
neighbors (x, y) = [(x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)]

day20a :: String -> Maybe Int
day20a input = flip runCont id $ callCC $ \exit ->
    bfsM id (go $ exit . Just) start $> Nothing where
    (maze, portals) = parse input
    reversePortals = invertMap portals
    [start] = reversePortals ! "AA"
    [end] = reversePortals ! "ZZ"
    go exit d p | p == end = exit d
    go _ _ p = return $ filter (`Set.member` maze) (neighbors p) ++ do
        portal <- maybeToList $ portals !? p
        filter (/= p) $ reversePortals ! portal

day20b :: String -> Maybe Int
day20b input = flip runCont id $ callCC $ \exit ->
    bfsM id (go $ exit . Just) (start, 0 :: Int) $> Nothing where
    (maze, portals) = parse input
    (xs, ys) = unzip $ Set.toList maze
    (minX, maxX) = (minimum xs, maximum xs)
    (minY, maxY) = (minimum ys, maximum ys)
    reversePortals = invertMap portals
    [start] = reversePortals ! "AA"
    [end] = reversePortals ! "ZZ"
    go exit d (p, 0) | p == end = exit d
    go _ _ (p@(x, y), n) = return $
        (map (, n) $ filter (`Set.member` maze) $ neighbors p) ++ do
            portal <- maybeToList $ portals !? p
            q <- filter (/= p) $ reversePortals ! portal
            if x `elem` [minX, maxX] || y `elem` [minY, maxY]
            then guard (n > 0) $> (q, n - 1)
            else return (q, n + 1)
