{-|
Module:         Day11
Description:    <https://adventofcode.com/2019/day/11 Day 11: Space Police>
-}
{-# LANGUAGE BlockArguments, FlexibleContexts, LambdaCase, NondecreasingIndentation, TypeApplications, ViewPatterns #-}
module Day11 (day11a, day11b) where

import Control.Arrow ((***))
import Control.Monad.ST (runST)
import Data.Bool (bool)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, filter, findWithDefault, insert, keysSet, size)
import qualified Data.Set as Set (elems, member)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Data.Void (Void)
import Intcode (evalIntcodeT, getOutput, setInput)
import Intcode.Vector (memory, parser)
import Text.Megaparsec (ParseErrorBundle, parse)

walk :: (Vector v e, Integral e) => Bool -> v e -> Map (Int, Int) Bool
walk start mem0 = runST $ memory mem0 >>=
    evalIntcodeT (loop 0 0 0 1 Map.empty) `flip` return (bool 0 1 start) where
    loop x y dx dy m = getOutput >>= maybe (return m) \color -> do
        setInput $ return $ bool 0 1 $ color /= 0
        let m' = Map.insert (x, y) (color /= 0) m
        getOutput >>= maybe (return m') \turn -> do
        let (dx', dy') = if turn == 0 then (-dy, dx) else (dy, -dx)
            position@(x', y') = (x + dx', y + dy')
        setInput $ return $ bool 0 1 $ Map.findWithDefault False position m
        loop x' y' dx' dy' m'

day11a :: String -> Either (ParseErrorBundle String Void) Int
day11a = fmap (Map.size . walk @Unboxed.Vector @Int False) . parse parser ""

day11b :: String -> Either (ParseErrorBundle String Void) String
day11b input = do
    result <- Map.keysSet . Map.filter id . walk @Unboxed.Vector @Int True <$>
        parse parser "" input
    let (minMaxRange -> xs, minMaxRange -> ys) = unzip $ Set.elems result
    return . drop 1 $ do
        y <- reverse ys
        '\n' : [bool '\x2591' '\x2593' $ Set.member (x, y) result | x <- xs]
  where
    minMaxRange = maybe [] (uncurry enumFromTo) . foldr minMaxRange' Nothing
    minMaxRange' a = Just . maybe (a, a) (min a *** max a)
