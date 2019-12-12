{-|
Module:         Day12
Description:    <https://adventofcode.com/2019/day/12 Day 12: The N-Body Problem>
-}
{-# LANGUAGE FlexibleContexts, TupleSections, TypeApplications #-}
module Day12 (day12a, day12b) where

import Data.List (findIndex, scanl', transpose)
import Data.Maybe (fromJust)
import qualified Data.Set as Set (empty, insert, member)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, between, parse, sepBy, sepEndBy, skipSome)
import Text.Megaparsec.Char (alphaNumChar, char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (Integral a, MonadParsec e String m) => m [[a]]
parser = transpose <$> line `sepEndBy` newline where
    line = between (char '<') (char '>') $ var `sepBy` string ", "
    var = skipSome alphaNumChar >> char '=' >> signed (return ()) decimal

sim :: (Num a) => [a] -> [[(a, a)]]
sim = iterate sim1 . map (, 0) where
    sim1 pvs =
      [ (p + v', v')
      | (p, v) <- pvs
      , let v' = v + sum [signum $ p' - p | (p', _) <- pvs]
      ]

cycleSize :: (Ord a) => [a] -> Int
cycleSize xs = fromJust . findIndex id . zipWith Set.member xs $
    scanl' (flip Set.insert) Set.empty xs

day12a :: String -> Either (ParseErrorBundle String ()) [Int]
day12a input = do
    states <- transpose . map sim <$> parse (parser @Int) "" input
    return
      [ sum [sum (abs <$> ps) * sum (abs <$> vs) | (ps, vs) <- unzip <$> moons]
      | moons <- transpose <$> states
      ]

day12b :: String -> Either (ParseErrorBundle String ()) Int
day12b = fmap (foldr lcm 1 . map (cycleSize . sim)) . parse (parser @Int) ""
