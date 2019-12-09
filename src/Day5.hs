{-|
Module:         Day5
Description:    <https://adventofcode.com/2019/day/5 Day 5: Sunny with a Chance of Asteroids>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module Day5 (day5a, day5b) where

import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, readArray, thaw, writeArray)
import Data.Array.Unboxed (IArray, UArray, listArray)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (last)
import Intcode (Memory(..), run)
import Text.Megaparsec (MonadParsec, parse, sepBy)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (signed, decimal)

parser :: (IArray a e, Integral e, MonadParsec err String m) => m (a Int e)
parser = do
    ints <- signed (return ()) decimal `sepBy` char ',' <* space
    return $ listArray (0, length ints - 1) ints

day5a :: String -> Maybe Int
day5a input = fmap NonEmpty.last . nonEmpty $ runST $ do
    mem <- parseST input
    run Memory { readMem = readArray mem, writeMem = writeArray mem } [1]
  where
    parseST :: String -> ST s (STUArray s Int Int)
    parseST = either (fail . show) thaw . parse (parser @UArray @Int @()) ""

day5b :: String -> Maybe Int
day5b input = fmap NonEmpty.last . nonEmpty $ runST $ do
    mem <- parseST input
    run Memory { readMem = readArray mem, writeMem = writeArray mem } [5]
  where
    parseST :: String -> ST s (STUArray s Int Int)
    parseST = either (fail . show) thaw . parse (parser @UArray @Int @()) ""
