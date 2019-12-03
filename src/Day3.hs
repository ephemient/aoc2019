{-|
Module:         Day3
Description:    <https://adventofcode.com/2019/day/3 Day 3: Crossed Wires>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications, ViewPatterns #-}
module Day3 (day3a, day3b) where

import Data.List (foldl', sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (delete, elems, empty, filter, fromListWith, keys, unionWith)
import Data.Maybe (mapMaybe)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, oneOf, parse, sepBy, sepEndBy)
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (Integral a, MonadParsec e String m) => m [[(Char, a)]]
parser = flip sepEndBy space1 . flip sepBy (char ',') $
    (,) <$> oneOf "ULDR" <*> decimal

walk :: (Enum k, Num k, Ord k) => [a] -> [(Char, k)] -> Map (k, k) a
walk = (.) (Map.delete o . Map.fromListWith (flip const)) . walk' o where
    o = (0, 0)
    walk' p es ((_, 0):rest) = walk' p es rest
    walk' p@(x, y) (e:es) ((c, n):rest) = (p, e):walk' p' es ((c, pred n):rest)
      where p' | 'U' <- c = (x, succ y)
               | 'L' <- c = (pred x, y)
               | 'D' <- c = (x, pred y)
               | 'R' <- c = (succ x, y)
               | otherwise = error "invalid direction"
    walk' _ _ _ = []

intersections :: (Ord k) => [Map k a] -> Map k [a]
intersections = Map.filter atLeastTwo . foldl' merge Map.empty where
    merge = (. fmap (:[])) . Map.unionWith (++)
    atLeastTwo (_:_:_) = True
    atLeastTwo _ = False

day3a :: String -> Either (ParseErrorBundle String ()) Int
day3a input = minimum . map manhattan . Map.keys . intersections .
    zipWith (walk . repeat) [0 :: Int ..] <$> parse parser "" input
  where manhattan (x, y) = abs x + abs y

day3b :: String -> Either (ParseErrorBundle String ()) Int
day3b input = minimum . mapMaybe shortest . Map.elems . intersections .
    map (walk [0..]) <$> parse (parser @Int) "" input where
    shortest (sort -> a:b:_) = Just $ a + b
    shortest _ = Nothing
