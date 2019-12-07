{-|
Module:         Day2
Description:    <https://adventofcode.com/2019/day/2 Day 2: 1202 Program Alarm>
-}
{-# LANGUAGE FlexibleContexts, NamedFieldPuns, RecordWildCards, TypeApplications #-}
module Day2 (day2a, day2b) where

import Control.Monad (unless)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray, STUArray, readArray, thaw, writeArray)
import Data.Array.Unboxed (Array, IArray, UArray, listArray)
import Data.Ix (inRange)
import Intcode (Memory(..), run)
import Text.Megaparsec (MonadParsec, parse, sepBy)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (IArray a e, Integral e, MonadParsec err String m) => m (a Int e)
parser = do
    ints <- decimal `sepBy` char ',' <* space
    return $ listArray (0, length ints - 1) ints

day2a :: String -> Int
day2a input = runST $ do
    mem <- parseST input
    writeArray mem 1 12
    writeArray mem 2 2
    [] <- run Memory { readMem = readArray mem, writeMem = writeArray mem } []
    readArray mem 0
  where
    parseST :: String -> ST s (STUArray s Int Int)
    parseST = either (fail . show) thaw . parse (parser @UArray @Int @()) ""

data XY i = XY {x :: !i, y :: !i, c :: !i} | XYError
instance (Num i, Eq i) => Num (XY i) where
    XY x1 y1 c1 + XY x2 y2 c2 = XY (x1 + x2) (y1 + y2) (c1 + c2)
    _ + _ = XYError
    XY 0 0 c1 * XY x y c2 = XY (c1 * x) (c1 * y) (c1 * c2)
    XY x y c1 * XY 0 0 c2 = XY (x * c2) (y * c2) (c1 * c2)
    _ * _ = XYError
    abs (XY 0 0 c) = XY 0 0 $ abs c
    abs _ = XYError
    signum (XY 0 0 c) = XY 0 0 $ signum c
    signum _ = XYError
    fromInteger = XY 0 0 . fromInteger
    negate XY {..} = XY (-x) (-y) (-c)
    negate _ = XYError
instance (Eq i) => Eq (XY i) where
    XY _ _ c1 == XY _ _ c2 = c1 == c2
    _ == _ = True
instance (Ord i) => Ord (XY i) where
    XY _ _ c1 `compare` XY _ _ c2 = compare c1 c2
    compare _ _ = EQ
instance (Enum i, Eq i, Num i) => Enum (XY i) where
    toEnum = XY 0 0 . toEnum
    fromEnum (XY 0 0 c) = fromEnum c
    fromEnum _ = 0
instance (Real i) => Real (XY i) where
    toRational (XY 0 0 c) = toRational c
    toRational _ = 0
instance (Integral i) => Integral (XY i) where
    XY 0 0 c1 `quotRem` XY 0 0 c2
      = let (q, r) = quotRem c1 c2 in (XY 0 0 q, XY 0 0 r)
    quotRem _ _ = (XYError, XYError)
    toInteger (XY 0 0 c) = toInteger c
    toInteger _ = 0

day2b :: String -> Int
day2b input = runST $ do
    mem <- parseST input
    writeArray mem 1 $ XY 1 0 0
    writeArray mem 2 $ XY 0 1 0
    [] <- run Memory { readMem = readArray mem, writeMem = writeArray mem } []
    XY {x = m, y = n, c} <- readArray mem 0
    unless (n == 1) $ fail "unhandled"
    let (x, y) = (19690720 - c) `quotRem` m
    unless (inRange (0, 99) x && inRange (0, 99) y) $ fail "unexpected"
    return $ 100 * x + y
  where
    parseST :: String -> ST s (STArray s Int (XY Int))
    parseST = either (fail . show) thaw . parse (parser @Array @(XY Int) @()) ""
