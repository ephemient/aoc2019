{-|
Module:         Day5
Description:    <https://adventofcode.com/2019/day/5 Day 5: Sunny with a Chance of Asteroids>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications, ViewPatterns #-}
module Day5 (day5a, day5b) where

import Control.Monad.ST (runST)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as Vector (fromList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Data.Void (Void)
import Intcode (run)
import Intcode.Vector (memory)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (signed, decimal)

parser :: (Vector v e, Integral e, MonadParsec err String m) => m (v e)
parser = Vector.fromList <$> signed (return ()) decimal `sepBy` char ','

diagnostic :: (Eq a, Num a) => [a] -> Maybe a
diagnostic (dropWhile (== 0) -> [a]) = Just a
diagnostic _ = Nothing

day5a :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day5a input = diagnostic <$> do
    mem <- parse (parser @Unboxed.Vector) "" input
    return $ runST $ memory mem >>= flip run [1]

day5b :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day5b input = diagnostic <$> do
    mem <- parse (parser @Unboxed.Vector) "" input
    return $ runST $ memory mem >>= flip run [5]
