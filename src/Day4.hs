{-|
Module:         Day4
Description:    <https://adventofcode.com/2019/day/4 Day 4: Secure Container>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Day4 (day4a, day4b) where

import Data.List (group)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, count, notFollowedBy, parse)
import Text.Megaparsec.Char (char, digitChar, space)

parser :: (MonadParsec e String m) => m (String, String)
parser = (,) <$> (num <* char '-') <*> (num <* space) where
    num = notFollowedBy (char '0') >> count 6 digitChar

makeNonDecreasing :: (Ord a) => [a] -> [a]
makeNonDecreasing (x:y:rest) = x : makeNonDecreasing (max x y : rest)
makeNonDecreasing xs = xs

nextNonDecreasing :: (Enum a, Eq a) => (a, a) -> [a] -> [a]
nextNonDecreasing (lo, hi) xs
  | (zs, y:ys) <- break (/= hi) $ reverse xs
  = let z = succ y in reverse ys ++ (z : map (const z) zs)
  | otherwise = lo : map (const lo) xs

rightAlignedCompare :: (Ord a) => [a] -> [a] -> Ordering
rightAlignedCompare x y = dropInits x y where
    dropInits [] [] = compare x y
    dropInits [] _ = LT
    dropInits _ [] = GT
    dropInits (_:xs) (_:ys) = dropInits xs ys

nonDecreasingRange :: (Enum a, Ord a) => (a, a) -> ([a], [a]) -> [[a]]
nonDecreasingRange bounds (start, end) =
    takeWhile ((/= LT) . rightAlignedCompare end) .
    iterate (nextNonDecreasing bounds) $ makeNonDecreasing start

day4a :: String -> Either (ParseErrorBundle String ()) Int
day4a input = length . filter ok . nonDecreasingRange ('1', '9') <$>
    parse parser "" input where
    ok xs@(_:ys) = or $ zipWith (==) xs ys
    ok _ = False

day4b :: String -> Either (ParseErrorBundle String ()) Int
day4b input = length . filter ok . nonDecreasingRange ('1', '9') <$>
    parse parser "" input where
    ok = any (\case [_, _] -> True; _ -> False) . group
