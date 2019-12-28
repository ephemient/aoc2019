{-|
Module:         Day5
Description:    <https://adventofcode.com/2019/day/5 Day 5: Sunny with a Chance of Asteroids>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications, ViewPatterns #-}
module Day5 (day5a, day5b) where

import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Data.Void (Void)
import Intcode.Vector (parser, run)
import Text.Megaparsec (ParseErrorBundle, parse)

diagnostic :: (Eq a, Num a) => [a] -> Maybe a
diagnostic (dropWhile (== 0) -> [a]) = Just a
diagnostic _ = Nothing

day5a :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day5a input = diagnostic <$> do
    mem0 <- parse (parser @Unboxed.Vector) "" input
    return $ runST $ run mem0 [1]

day5b :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day5b input = diagnostic <$> do
    mem0 <- parse (parser @Unboxed.Vector) "" input
    return $ runST $ run mem0 [5]
