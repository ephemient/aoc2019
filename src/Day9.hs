{-|
Module:         Day9
Description:    <https://adventofcode.com/2019/day/9 Day 9: Sensor Boost>
-}
{-# LANGUAGE FlexibleContexts #-}
module Day9 (day9a, day9b) where

import Control.Monad.ST (runST)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap ((!?), fromDistinctAscList, insert)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (last)
import Data.Maybe (fromMaybe)
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import Intcode (Memory(..), run)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (Integral e, MonadParsec err String m) => m (IntMap e)
parser = IntMap.fromDistinctAscList . zip [0..] <$>
    (signed (return ()) decimal `sepBy` char ',' <* space)

runOnIntMap :: (Integral e) => IntMap e -> [e] -> [e]
runOnIntMap mem0 input = runST $ do
    mem <- newSTRef mem0
    base <- newSTRef 0
    let memory = Memory
          { readMem = \n -> fromMaybe 0 . (IntMap.!? n) <$> readSTRef mem
          , writeMem = \n v -> modifySTRef' mem . IntMap.insert n $! v
          , readBase = readSTRef base
          , modifyBase = modifySTRef' base
          }
    run memory input

day9a :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day9a = fmap (fmap NonEmpty.last . nonEmpty . flip runOnIntMap [1]) .
    parse parser ""

day9b :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day9b = fmap (fmap NonEmpty.last . nonEmpty . flip runOnIntMap [2]) .
    parse parser ""
