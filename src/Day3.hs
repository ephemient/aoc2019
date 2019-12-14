{-|
Module:         Day3
Description:    <https://adventofcode.com/2019/day/3 Day 3: Crossed Wires>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications, TypeFamilies #-}
module Day3 (day3a, day3b) where

import Control.Arrow (first, second)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map (elems, fromDistinctAscList, fromDistinctDescList, keysSet, intersectionWith, unionsWith)
import Data.Functor (($>))
import Data.List (foldl1', mapAccumL)
import Data.Maybe (mapMaybe)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Set as Set (intersection, lookupMin, map, unions)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, choice, parse, sepBy, sepEndBy)
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (Integral a, MonadParsec e String m, Show a) => m [Map (a, a) a]
parser = line `sepEndBy` space1 where
    line = Map.unionsWith const . snd . mapAccumL walk ((0, 0), 0) <$> segments
    segments = ((,) <$> dir <*> decimal) `sepBy` char ','
    dir = choice
      [ char 'U' $> (Map.fromDistinctAscList, second . (+))
      , char 'L' $> (Map.fromDistinctDescList, first . subtract)
      , char 'D' $> (Map.fromDistinctDescList, second . subtract)
      , char 'R' $> (Map.fromDistinctAscList, first . (+))
      ]
    walk (p, d) ((fromList, go), n) =
        ((go n p, d + n), fromList [(go i p, d + i) | i <- [1..n]])

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose n (x:xs) = ((x :) <$> choose (n - 1) xs) ++ choose n xs
choose _ [] = []

day3a :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day3a input = Set.lookupMin . Set.unions .
    map (Set.map manhattan . foldl1' Set.intersection) . choose 2 .
    map Map.keysSet <$> parse parser "" input where
    manhattan (x, y) = abs x + abs y

day3b :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day3b input = minimum' .
    mapMaybe (minimum' . Map.elems . foldl1' (Map.intersectionWith (+))) .
    choose 2 <$> parse (parser @Int) "" input where
    minimum' = fmap minimum . nonEmpty
