{-|
Module:         Day12
Description:    <https://adventofcode.com/2019/day/12 Day 12: The N-Body Problem>
-}
{-# LANGUAGE FlexibleContexts, TupleSections, TypeApplications #-}
module Day12 (day12a, day12b) where

import Data.List (elemIndex, transpose)
import Data.Maybe (catMaybes)
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

day12a :: String -> Either (ParseErrorBundle String ()) [Int]
day12a input = do
    states <- transpose . map sim <$> parse parser "" input
    return
      [ sum [sum (abs <$> ps) * sum (abs <$> vs) | (ps, vs) <- unzip <$> moons]
      | moons <- transpose <$> states
      ]

day12b :: String -> Either (ParseErrorBundle String ()) Int
day12b input = do
    initial <- parse (parser @Int) "" input
    return . foldr (lcm . succ) 1 . catMaybes .
        zipWith (elemIndex . map (, 0)) initial $ tail . sim <$> initial
