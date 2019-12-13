{-|
Module:         Day2
Description:    <https://adventofcode.com/2019/day/2 Day 2: 1202 Program Alarm>
-}
{-# LANGUAGE FlexibleContexts, NamedFieldPuns, RecordWildCards, TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day2 (day2a, day2b) where

import Control.Monad (unless)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray, STUArray, readArray, thaw, writeArray)
import Data.Array.Unboxed (Array, IArray, UArray, listArray)
import Data.Ix (inRange)
import Intcode (Memory(..))
import qualified Intcode (run)
import qualified Intcode.Array (run)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (IArray a e, Integral e, MonadParsec err String m) => m (a Int e)
parser = do
    ints <- decimal `sepBy` char ',' <* space
    return $ listArray (0, length ints - 1) ints

day2a :: String -> Either (ParseErrorBundle String ()) Int
day2a input = do
    mem0 <- parse (parser @UArray) "" input
    return $ runST $ do
        mem <- thaw mem0
        writeArray mem 1 12
        writeArray mem 2 2
        [] <- run' mem []
        readArray mem 0
  where run' = Intcode.Array.run :: STUArray s Int Int -> [Int] -> ST s [Int]

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
instance (Ord i) => Ord (XY i) where
    XY _ _ c1 `compare` XY _ _ c2 = compare c1 c2
instance (Enum i, Eq i, Num i) => Enum (XY i) where
    toEnum = XY 0 0 . toEnum
    fromEnum (XY 0 0 c) = fromEnum c
instance (Real i) => Real (XY i) where
    toRational (XY 0 0 c) = toRational c
instance (Integral i) => Integral (XY i) where
    XY 0 0 c1 `quotRem` XY 0 0 c2 =
        let (q, r) = quotRem c1 c2 in (XY 0 0 q, XY 0 0 r)
    quotRem _ _ = (XYError, XYError)
    toInteger (XY 0 0 c) = toInteger c

day2b :: String -> Either (ParseErrorBundle String ()) Int
day2b input = do
    mem0 <- parse (parser @Array @(XY Int)) "" input
    return $ runST $ do
        mem <- thaw' mem0
        writeArray mem 1 $ XY 1 0 0
        writeArray mem 2 $ XY 0 1 0
        let readMem (XY 0 0 c) = readArray mem c
            readMem _ = return XYError
            writeMem = writeArray mem . fromIntegral
        [] <- Intcode.run Memory {..} []
        XY {x = m, y = n, c} <- readArray mem 0
        unless (n == 1) $ fail "unhandled"
        let (x, y) = (19690720 - c) `quotRem` m
        unless (inRange (0, 99) x && inRange (0, 99) y) $ fail "unexpected"
        return $ 100 * x + y
  where thaw' = thaw :: Array Int (XY Int) -> ST s (STArray s Int (XY Int))
