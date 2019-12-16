{-|
Module:         Day16
Description:    <https://adventofcode.com/2019/day/16 Day 16: Flawed Frequency Transmission>
-}
{-# LANGUAGE BangPatterns, TypeApplications, ViewPatterns #-}
module Day16 (day16a, day16b) where

import Data.Char (digitToInt, intToDigit, isDigit)
import Data.List (foldl')
import qualified Data.Vector.Generic as Vector (fromListN, map, postscanr', take, toList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)

f :: [Int] -> [Int]
f input =
  [ abs (sum $ zipWith ($) fs input) `mod` 10
  | (n, _) <- zip [1..] input
  , let _:fs = cycle $ concatMap (replicate n) [const 0, id, const 0, negate]
  ]

day16a :: String -> [String]
day16a (map digitToInt . filter isDigit -> input) =
    map intToDigit . take 8 <$> iterate f input

day16b :: String -> [String]
day16b (map digitToInt . filter isDigit -> input)
  | offset + 8 < extendedLength && extendedLength <= 2 * offset
  = map intToDigit . Vector.toList . Vector.take 8 <$> iterate g v0
  | otherwise = error "unimplemented!" where
    offset = foldl' (\a b -> 10 * a + b) 0 $ take 7 input
    realLength = length input
    extendedLength = realLength * 10000
    v0 = Vector.fromListN @Unboxed.Vector (extendedLength - offset) .
        drop (offset `mod` realLength) $ cycle input
    g = Vector.map ((`mod` 10) . abs) . Vector.postscanr' (+) 0
