{-|
Module:         Day2
Description:    <https://adventofcode.com/2019/day/2 Day 2: 1202 Program Alarm>
-}
{-# LANGUAGE FlexibleContexts, NamedFieldPuns, RecordWildCards, TypeApplications, ViewPatterns #-}
module Day2 (day2a, day2b) where

import Control.Monad (guard)
import Control.Monad.ST (runST)
import Data.Bool (bool)
import Data.Ix (inRange)
import qualified Data.Map.Lazy as Map (assocs, null, singleton)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as Boxed (Vector)
import Data.Vector.Generic (Vector, (//))
import qualified Data.Vector.Generic as Vector (fromList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Data.Void (Void)
import Intcode (Memory(..), run)
import Intcode.Vector (memory)
import Linear(Linear(..))
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (Vector v e, Integral e, MonadParsec err String m) => m (v e)
parser = Vector.fromList <$> decimal `sepBy` char ','

day2a :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day2a input = do
    mem0 <- parse (parser @Unboxed.Vector) "" input
    return $ runST $ do
        mem <- memory $ mem0 // [(1, 12), (2, 2)]
        null <$> run mem [] >>= bool (return Nothing) (Just <$> readMem mem 0)

day2b :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day2b input = do
    mem0 <- parse (parser @Boxed.Vector) "" input
    return $ do
        Linear {variables = Map.assocs -> [(0, m), (1, 1)], ..} <- runST $ do
            Memory {..} <- memory $ mem0 //
              [ (1, Linear 0 $ Map.singleton 0 1)
              , (2, Linear 0 $ Map.singleton 1 1)
              ]
            counter <- newSTRef (2 :: Int)
            let mem' = Memory
                  { readMem = \i@Linear {variables} ->
                        if Map.null variables then readMem i else do
                            n <- readSTRef counter
                            writeSTRef counter $! n + 1
                            return $ Linear 0 $ Map.singleton n 1
                  , writeMem = writeMem . fromIntegral
                  }
            null <$> run mem' [] >>= bool (return Nothing) (Just <$> readMem 0)
        let (x, y) = (19690720 - constant) `divMod` m
        guard $ inRange (0, 99) x && inRange (0, 99) y
        return $ 100 * x + y
