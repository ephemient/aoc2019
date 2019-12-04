{-|
Module:         Day4
Description:    <https://adventofcode.com/2019/day/4 Day 4: Secure Container>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications, ViewPatterns #-}
module Day4 (day4a, day4b) where

import Data.List (group)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (Integral a, MonadParsec e String m) => m [a]
parser = enumFromTo <$> (decimal <* char '-') <*> (decimal <* space)

isNonDecreasing :: (Ord a) => [a] -> Bool
isNonDecreasing xs@(_:ys) = and $ zipWith (<=) xs ys
isNonDecreasing _ = True

day4a :: String -> Either (ParseErrorBundle String ()) Int
day4a input = length . filter ok <$> parse (parser @Int) "" input where
    ok (show -> s) = isNonDecreasing s && any atLeastTwo (group s)
    atLeastTwo (_:_:_) = True
    atLeastTwo _ = False

day4b :: String -> Either (ParseErrorBundle String ()) Int
day4b input = length . filter ok <$> parse (parser @Int) "" input where
    ok (show -> s) = isNonDecreasing s && any exactlyTwo (group s)
    exactlyTwo [_, _] = True
    exactlyTwo _ = False
