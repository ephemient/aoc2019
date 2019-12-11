{-|
Module:         Day11
Description:    <https://adventofcode.com/2019/day/11 Day 11: Space Police>
-}
{-# LANGUAGE FlexibleContexts, NamedFieldPuns, RecordWildCards, TypeApplications #-}
module Day11 (day11a, day11b) where

import Control.Monad.ST (runST)
import Control.Monad.State (evalStateT, get, gets, lift, put)
import Data.Bool (bool)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, filter, findWithDefault, insert, keysSet, size)
import qualified Data.Set as Set (elems, member)
import Data.Vector.Generic (Vector, fromList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Intcode (Context(..), Memory(..), step)
import Intcode.Vector (memory)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (Vector v e, Integral e, MonadParsec err String m) => m (v e)
parser = fromList <$> (signed (return ()) decimal `sepBy` char ',' <* space)

data WalkState k = WalkState
  { grid :: Map (k, k) Bool
  , position :: (k, k)
  , direction :: (k, k)
  }

walk :: (Num k, Ord k, Vector v Int) => Bool -> v Int -> Map (k, k) Bool
walk start mem0 = runST $ do
    Memory {..} <- memory mem0
    let mem = Memory
          { readMem = lift . readMem
          , writeMem = (.) lift . writeMem
          }
        context0 = Context
          { next = step mem context0
          , output = \color input base ip -> do
                walkState@WalkState {grid, position} <- get
                put walkState {grid = Map.insert position (color /= 0) grid}
                next context1 input base ip
          , terminate
          }
        context1 = Context
          { next = step mem context1
          , output = \turn input base ip -> do
                walkState@WalkState {..} <- get
                let (x, y) = position
                    (dx, dy) = direction
                    d@(dx', dy') = if turn == 0 then (-dy, dx) else (dy, -dx)
                    p = (x + dx', y + dy')
                    color = Map.findWithDefault False p grid
                put walkState {position = p, direction = d}
                next context0 (input ++ [fromEnum color]) base ip
          , terminate
          }
        terminate _ _ _ = gets grid
    evalStateT (step mem context0 [fromEnum start] 0 0)
        WalkState { grid = Map.empty, position = (0, 0), direction = (0, 1) }

day11a :: String -> Either (ParseErrorBundle String ()) Int
day11a = fmap (Map.size . walk @Int @Unboxed.Vector False) . parse parser ""

day11b :: String -> Either (ParseErrorBundle String ()) String
day11b input = do
    result <- Map.keysSet . Map.filter id . walk @Int @Unboxed.Vector True <$>
        parse parser "" input
    let (xs, ys) = unzip $ Set.elems result
    return $ intercalate "\n"
      [ [ bool '\x2591' '\x2593' $ (x, y) `Set.member` result
        | x <- [minimum xs..maximum xs]
        ]
      | y <- reverse [minimum ys..maximum ys]
      ]
