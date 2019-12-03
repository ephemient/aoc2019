{-|
Module:         Day1
Description:    <https://adventofcode.com/2019/day/1 Day 1: The Tyranny of the Rocket Equation>
-}
module Day1 (day1a, day1b) where

fuel :: (Integral a) => a -> a
fuel = subtract 2 . flip div 3

fuels :: (Integral a) => a -> [a]
fuels = takeWhile (> 0) . iterate fuel . fuel

day1a :: String -> Integer
day1a = sum . map (fuel . read) . lines

day1b :: String -> Integer
day1b = sum . concatMap (fuels . read) . lines
