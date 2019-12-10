{-|
Module:         Day10
Description:    <https://adventofcode.com/2019/day/10 Day 10: Monitoring Station>
-}
{-# LANGUAGE ViewPatterns #-}
module Day10 (day10a, day10b) where

import Data.List (inits, maximumBy, sort, tails, transpose)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map (elems, fromListWith, size)
import Data.Ord (comparing)
import Data.Ratio (Ratio, (%))

parse :: String -> [(Int, Int)]
parse input =
    [(x, y) | (y, line) <- zip [0..] $ lines input, (x, '#') <- zip [0..] line]

type Direction a = (Bool, Maybe (Ratio a))

sight :: (Integral a) => (a, a) -> (a, a) -> (Direction a, a)
sight (x, y) (u, v)
  | x == u = ((y < v, Nothing), abs $ y - v)
  | otherwise = ((x > u, Just $ (y - v) % (x - u)), abs $ x - u)

best :: (Integral a) => [(a, a)] -> Map (Direction a) [(a, (a, a))]
best points = maximumBy (comparing Map.size)
  [ fmap sort . Map.fromListWith (++) $ annotate <$> before ++ after
  | (before, point:after) <- zip (inits points) (tails points)
  , let annotate point'@(sight point -> (dir, dist)) = (dir, [(dist, point')])
  ]

day10a :: String -> Int
day10a = Map.size . best . parse

day10b :: String -> [Int]
day10b = map format . concat . transpose . Map.elems . best . parse where
    format (_, (x, y)) = 100 * x + y
