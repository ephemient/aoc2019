{-|
Module:         Day19
Description:    <https://adventofcode.com/2019/day/19 Day 19: Tractor Beam>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module Day19 (day19a, day19b) where

import Control.Monad.ST (runST)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Data.Void (Void)
import Intcode.Vector (parser, run)
import Text.Megaparsec (ParseErrorBundle, parse)

program :: (Vector v Int) => v Int -> Int -> Int -> Bool
program mem0 x y = runST $ do
    [ret] <- run mem0 [x, y]
    return $ toEnum ret

day19a :: String -> Either (ParseErrorBundle String Void) Int
day19a input = do
    mem0 <- parse (parser @Unboxed.Vector) "" input
    return $ length [() | x <- [0..49], y <- [0..49], program mem0 x y]

day19b :: String -> Either (ParseErrorBundle String Void) Int
day19b input = do
    mem0 <- parse (parser @Unboxed.Vector) "" input
    let walk x y
          | not $ program mem0 x y = walk (x + 1) y
          | not $ program mem0 (x + 99) (y - 99) = walk x (y + 1)
          | otherwise = 10000 * x + y - 99
    return $ walk 0 99
