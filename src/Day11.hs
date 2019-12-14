{-|
Module:         Day11
Description:    <https://adventofcode.com/2019/day/11 Day 11: Space Police>
-}
{-# LANGUAGE BlockArguments, FlexibleContexts, LambdaCase, NondecreasingIndentation, TypeApplications #-}
module Day11 (day11a, day11b) where

import Control.Monad.ST (runST)
import Data.Bool (bool)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, filter, findWithDefault, insert, keysSet, size)
import qualified Data.Set as Set (elems, member)
import Data.Vector.Generic (Vector, fromList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Data.Void (Void)
import Intcode (evalIntcodeT, getOutput, setInput)
import Intcode.Vector (memory)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (Vector v e, Integral e, MonadParsec err String m) => m (v e)
parser = fromList <$> (signed (return ()) decimal `sepBy` char ',' <* space)

walk :: (Integral e, Vector v e) => Bool -> v e -> Map (Int, Int) Bool
walk start mem0 = runST $ do
    mem <- memory mem0
    let loop x y dx dy m = getOutput >>= maybe (return m) \color -> do
            setInput $ return $ bool 0 1 $ color /= 0
            let m' = Map.insert (x, y) (color /= 0) m
            getOutput >>= maybe (return m') \turn -> do
            let (dx', dy') = if turn == 0 then (-dy, dx) else (dy, -dx)
                position@(x', y') = (x + dx', y + dy')
            setInput $ return $ bool 0 1 $ Map.findWithDefault False position m
            loop x' y' dx' dy' m'
    evalIntcodeT (loop 0 0 0 1 Map.empty) mem $ return $ bool 0 1 start

day11a :: String -> Either (ParseErrorBundle String Void) Int
day11a = fmap (Map.size . walk @Int @Unboxed.Vector False) . parse parser ""

day11b :: String -> Either (ParseErrorBundle String Void) String
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
