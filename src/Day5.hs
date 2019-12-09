{-|
Module:         Day5
Description:    <https://adventofcode.com/2019/day/5 Day 5: Sunny with a Chance of Asteroids>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module Day5 (day5a, day5b) where

import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, thaw)
import Data.Array.Unboxed (IArray, UArray, listArray)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (last)
import Intcode.Array (run)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (signed, decimal)

parser :: (IArray a e, Integral e, MonadParsec err String m) => m (a Int e)
parser = do
    ints <- signed (return ()) decimal `sepBy` char ',' <* space
    return $ listArray (0, length ints - 1) ints

day5a :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day5a input = fmap NonEmpty.last . nonEmpty <$> do
    mem <- parse (parser @UArray) "" input
    return $ runST $ thaw mem >>= flip run' [1]
  where run' = run :: STUArray s Int Int -> [Int] -> ST s [Int]

day5b :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day5b input = fmap NonEmpty.last . nonEmpty <$> do
    mem <- parse (parser @UArray) "" input
    return $ runST $ thaw mem >>= flip run' [5]
  where run' = run :: STUArray s Int Int -> [Int] -> ST s [Int]
