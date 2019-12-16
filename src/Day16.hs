{-|
Module:         Day16
Description:    <https://adventofcode.com/2019/day/16 Day 16: Flawed Frequency Transmission>
-}
{-# LANGUAGE FlexibleContexts, TransformListComp, TypeApplications, ViewPatterns #-}
module Day16 (day16a, day16b) where

import Control.Monad (foldM, forM_)
import Data.Char (digitToInt, intToDigit, isDigit)
import Data.List (foldl')
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as Vector (drop, foldl', fromList, fromListN, modify, take, toList, zipWith)
import qualified Data.Vector.Generic.Mutable as MVector (length, read, write)
import qualified Data.Vector.Unboxed as Unboxed (Vector)

f :: (Vector v e, Integral e) => v e -> v e
f = Vector.modify $ \v -> let n = MVector.length v in forM_ [0..n - 1] $ \i ->
    (`mod` 10) . abs <$> foldM (acc v) 0 (fns i n) >>= MVector.write v i where
    acc v k (fn, i) = (+) k . fn <$> MVector.read v i
    fns x n =
      [ (fn, base + i)
      | (base, fn) <- zip [x, 3 * x + 2..] $ cycle [id, negate]
      , i <- [0..x]
      , then takeWhile by base + i < n
      ]

day16a :: String -> [String]
day16a (map digitToInt . filter isDigit -> input) =
    map intToDigit . Vector.toList . Vector.take 8 <$> iterate f v0 where
    v0 = Vector.fromList @Unboxed.Vector input

m :: [Int]
m = fromIntegral . (`mod` 10) <$>
    scanl (\x i -> x * (99 + i) `div` i) 1 [1 :: Integer ..]

day16b :: String -> String
day16b (map digitToInt . filter isDigit -> input)
  | 8 <= n && n < offset = intToDigit .  Vector.foldl' (+:) 0 .
        Vector.zipWith (*:) m' . flip Vector.drop v0 <$> [0..7]
  | otherwise = error "unimplemented!" where
    offset = foldl' (\a b -> 10 * a + b) 0 $ take 7 input
    n = length input * 10000 - offset
    v0 = Vector.fromListN @Unboxed.Vector n .
        drop (offset `mod` length input) $ cycle input
    m' = Vector.fromListN n m
    a +: b = (a + b) `mod` 10
    a *: b = (a * b) `mod` 10
