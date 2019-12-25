{-|
Module:         Day21
Description:    <https://adventofcode.com/2019/day/21 Day 21: Springboard Adventure>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module Day21 (Command(..), Register(..), day21a, day21b, programA, programB) where

import Control.Monad.ST (runST)
import Data.List (init)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as Vector (fromList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Data.Void (Void)
import Intcode.Char (runAsciiTraceUntilInt)
import Intcode.Vector (memory)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Register = A | B | C | D | E | F | G | H | I | J | T
  deriving (Enum, Eq, Ord, Show)
data Command a = AND a a | OR a a | NOT a a | WALK | RUN deriving (Eq, Show)

parser :: (Vector v e, Integral e, MonadParsec err String m) => m (v e)
parser = Vector.fromList <$> signed (return ()) decimal `sepBy` char ','

programA, programB :: [Command Register]
programA = [OR A T, AND B T, AND C T, NOT T J, AND D J, WALK]
programB = init programA ++ [NOT J T, OR E T, OR H T, AND T J, RUN]

day21a :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day21a input = do
    mem0 <- parse (parser @Unboxed.Vector) "" input
    return $ runST $ do
        mem <- memory mem0
        runAsciiTraceUntilInt mem . unlines $ show <$> programA

day21b :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day21b input = do
    mem0 <- parse (parser @Unboxed.Vector) "" input
    return $ runST $ do
        mem <- memory mem0
        runAsciiTraceUntilInt mem . unlines $ show <$> programB
