{-|
Module:         Day21
Description:    <https://adventofcode.com/2019/day/21 Day 21: Springboard Adventure>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module Day21 (day21a, day21b) where

import Control.Monad.ST (runST)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (last)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as Vector
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Data.Void (Void)
import Intcode.Char (runTraced)
import Intcode.Vector (memory)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (Vector v e, Integral e, MonadParsec err String m) => m (v e)
parser = Vector.fromList <$> signed (return ()) decimal `sepBy` char ','

day21a :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day21a input = do
    mem0 <- parse (parser @Unboxed.Vector) "" input
    return . fmap NonEmpty.last . nonEmpty $ runST $ do
        mem <- memory mem0
        runTraced mem $ unlines
          [ "OR A T"
          , "AND B T"
          , "AND C T"
          , "NOT T J"
          , "AND D J"
          , "WALK"
          ]

day21b :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day21b input = do
    mem0 <- parse (parser @Unboxed.Vector) "" input
    return . fmap NonEmpty.last . nonEmpty $ runST $ do
        mem <- memory mem0
        runTraced mem $ unlines
          [ "OR A T"
          , "AND B T"
          , "AND C T"
          , "NOT T J"
          , "AND D J"
          , "NOT J T"
          , "OR E T"
          , "OR H T"
          , "AND T J"
          , "RUN"
          ]
