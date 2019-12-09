{-|
Module:         Day9
Description:    <https://adventofcode.com/2019/day/9 Day 9: Sensor Boost>
-}
{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module Day9 (day9a, day9b) where

import Control.Monad.ST (runST)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (last)
import Data.Vector.Unboxed (Unbox, Vector, fromList, thaw)
import Intcode.Vector (run)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (Integral e, Unbox e, MonadParsec err String m) => m (Vector e)
parser = fromList <$> (signed (return ()) decimal `sepBy` char ',' <* space)

day9a :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day9a input = fmap NonEmpty.last . nonEmpty <$> do
    mem <- parse parser "" input
    return $ runST $ thaw mem >>= flip run [1]

day9b :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day9b input = fmap NonEmpty.last . nonEmpty <$> do
    mem <- parse parser "" input
    return $ runST $ thaw mem >>= flip run [2]
