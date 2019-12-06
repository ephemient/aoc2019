{-|
Module:         Day6
Description:    <https://adventofcode.com/2019/day/6 Day 6: Universal Orbit Map>
-}
{-# LANGUAGE ViewPatterns #-}
module Day6 (day6a, day6b) where

import Control.Arrow (second)
import Control.Monad (join)
import Data.Function (on)
import Data.List (unfoldr)
import qualified Data.Map.Lazy as Map (fromList, fromListWith, lookup)
import Data.Tuple (swap)

parse :: String -> Maybe [(String, String)]
parse = mapM parse' . lines where
    parse' (break (== ')') -> (x, ')':y)) = Just (x, y)
    parse' _ = Nothing

day6a :: String -> Maybe Int
day6a input = do
    orbits <- Map.fromListWith (++) . map (second (:[])) <$> parse input
    let checksums = checksum <$> orbits
        checksum = sum . map (maybe 1 (+ 1) . flip Map.lookup checksums)
    return $ sum checksums

day6b :: String -> Maybe Int
day6b input = do
    rorbits <- Map.fromList . fmap swap <$> parse input
    let path = (reverse .) . unfoldr $ fmap (join (,)) . flip Map.lookup rorbits
        dropCommonPrefix (x:xs) (y:ys) | x == y = dropCommonPrefix xs ys
        dropCommonPrefix xs ys = (xs, ys)
    return . uncurry ((+) `on` length) $
        dropCommonPrefix (path "SAN") (path "YOU")
