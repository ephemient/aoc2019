{-|
Module:         Day3
Description:    <https://adventofcode.com/2019/day/3 Day 3: Crossed Wires>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications, TypeFamilies, ViewPatterns #-}
module Day3 (day3a, day3b) where

import Control.Arrow (first, second)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, empty, keysSet, insertWith, intersectionWith)
import Data.Functor (($>))
import Data.List (foldl', foldl1')
import Data.Maybe (mapMaybe)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Set as Set (intersection, lookupMin, map, unions)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, choice, option, parse, sepEndBy)
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (Integral a, MonadParsec e String m, Show a) => m [Map (a, a) Int]
parser = line Map.empty (0, 0) 1 `sepEndBy` space1 where
    dir = choice $ zipWith (($>) . char) "ULDR"
        [second succ, first pred, second pred, first succ]
    line m p d = do
        f <- dir
        n <- decimal
        let points@(last -> p') = take n . tail $ iterate f p
            m' = foldl' (flip . uncurry . Map.insertWith $ flip const) m $
                zip points [d..]
        option m' $ char ',' *> line m' p' (d + n)

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose n (x:xs) = ((x :) <$> choose (n - 1) xs) ++ choose n xs
choose _ [] = []

day3a :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day3a input = Set.lookupMin . Set.unions .
    map (Set.map manhattan . foldl1' Set.intersection) . choose 2 .
    map Map.keysSet <$> parse parser "" input where
    manhattan (x, y) = abs x + abs y

day3b :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day3b input = minimum' .
    mapMaybe (minimum' . Map.elems . foldl1' (Map.intersectionWith (+))) .
    choose 2 <$> parse (parser @Int) "" input where
    minimum' = fmap minimum . nonEmpty
