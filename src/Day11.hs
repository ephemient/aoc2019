{-|
Module:         Day11
Description:    <https://adventofcode.com/2019/day/11 Day 11: Space Police>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module Day11 (day11a, day11b) where

import Control.Monad.Fix (fix)
import Control.Monad.ST (runST)
import Data.Bool (bool)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, filter, findWithDefault, insert, keysSet, size)
import qualified Data.Set as Set (elems, member)
import Data.Vector.Generic (Vector, fromList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Intcode (Context(..), step)
import Intcode.Vector (memory)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (Vector v e, Integral e, MonadParsec err String m) => m (v e)
parser = fromList <$> (signed (return ()) decimal `sepBy` char ',' <* space)

walk :: (Num k, Ord k, Vector v Int) => Bool -> v Int -> Map (k, k) Bool
walk start mem0 = runST $ do
    mem <- memory mem0
    let context0 grid position direction = fix $ \context -> Context
          { next = step mem context
          , output = \color _ base ip -> do
                let grid' = Map.insert position (color /= 0) grid
                next (context1 grid' position direction) (repeat color) base ip
          , terminate = \_ _ _ -> return grid
          }
        context1 grid (x, y) (dx, dy) = fix $ \context -> Context
          { next = step mem context
          , output = \turn _ base ip -> do
                let direction@(dx', dy') =
                        if turn == 0 then (-dy, dx) else (dy, -dx)
                    position = (x + dx', y + dy')
                    color = fromEnum $ Map.findWithDefault False position grid
                next (context0 grid position direction) (repeat color) base ip
          , terminate = \_ _ _ -> return grid
          }
    step mem (context0 Map.empty (0, 0) (0, 1)) (repeat $ fromEnum start) 0 0

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
