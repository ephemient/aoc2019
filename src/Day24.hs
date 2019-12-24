{-|
Module:         Day24
Description:    <https://adventofcode.com/2019/day/24 Day 24: Planet of Discord>
-}
{-# LANGUAGE TupleSections #-}
module Day24 (day24a, day24b) where

import Common (findCycle, neighbors)
import Data.Ix (inRange, range)
import qualified Data.Map as Map (filterWithKey, fromListWith, keysSet)
import Data.Set (Set)
import qualified Data.Set as Set (fromDistinctAscList, mapMonotonic, member, size)

parse :: String -> Set (Int, Int)
parse input = Set.fromDistinctAscList $ do
    (y, line) <- zip [-2..2] $ lines input
    (x, '#') <- zip [-2..2] line
    return (y, x)

evolve :: (Ord a) => (a -> [a]) -> Set a -> Set a
evolve f s = Map.keysSet . Map.filterWithKey alive . Map.fromListWith (+) $
    (, 1 :: Int) <$> foldMap f s where
    alive _ 1 = True
    alive p 2 = not $ Set.member p s
    alive _ _ = False

withDepth :: Int -> (Int, Int) -> (Int, Int, Int)
withDepth z (x, y) = (x, y, z)

neighborsA :: (Int, Int) -> [(Int, Int)]
neighborsA = filter (inRange ((-2, -2), (2, 2))) . neighbors

neighborsB ::  (Int, Int, Int) -> [(Int, Int, Int)]
neighborsB (x, y, z) =
    map (withDepth z) (filter (/= (0, 0)) $ neighborsA (x, y)) ++
    [(x', 2 * y, z - 1) | x == 0, abs y == 1, x' <- [-2..2]] ++
    [(2 * x, y', z - 1) | abs x == 1, y == 0, y' <- [-2..2]] ++
    [(signum x, 0, z + 1) | abs x == 2] ++ [(0, signum y, z + 1) | abs y == 2]

day24a :: String -> Maybe Int
day24a = fmap count . findCycle . iterate (evolve neighborsA) . parse where
    count s = sum
      [ n
      | (p, n) <- zip (range ((-2, -2), (2, 2))) (iterate (* 2) 1)
      , Set.member p s
      ]

day24b :: String -> [Int]
day24b = fmap Set.size . iterate (evolve neighborsB) .
    Set.mapMonotonic (withDepth 0) . parse
