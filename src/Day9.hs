{-|
Module:         Day9
Description:    <https://adventofcode.com/2019/day/9 Day 9: Sensor Boost>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module Day9 (day9a, day9b) where

import Control.Monad.ST (runST)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (last)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Data.Void (Void)
import Intcode.Vector (parser, run)
import Text.Megaparsec (ParseErrorBundle, parse)

day9a :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day9a input = fmap NonEmpty.last . nonEmpty <$> do
    mem0 <- parse (parser @Unboxed.Vector) "" input
    return $ runST $ run mem0 [1]

day9b :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day9b input = fmap NonEmpty.last . nonEmpty <$> do
    mem0 <- parse (parser @Unboxed.Vector) "" input
    return $ runST $ run mem0 [2]
