{-|
Module:         Day17
Description:    <https://adventofcode.com/2019/day/17 Day 17: Set and Forget>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, TransformListComp, TypeApplications #-}
module Day17 (day17a, day17b) where

import Control.Arrow (first)
import Control.Monad.ST (runST)
import Data.Char (chr, ord)
import Data.Function (on)
import Data.Functor (($>))
import Data.List (groupBy, inits, stripPrefix, tails)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set (fromList, intersection, size)
import Data.Vector.Generic (Vector, (//))
import qualified Data.Vector.Generic as Vector (fromList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Data.Void (Void)
import Intcode (evalIntcodeT, getOutput, setInput)
import Intcode.Vector (memory)
import qualified Intcode.Vector (run)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal, signed)

import Debug.Trace

data Command = CommandLeft | CommandRight | CommandStep deriving (Eq)
instance Show Command where
    showsPrec _ CommandLeft s = 'L':s
    showsPrec _ CommandRight s = 'R':s
    showsPrec _ CommandStep s = '1':s
    showList = (drop 1 .) . showList' where
        showList' [] s = ',':'0':s
        showList' commands s = foldr showGroup s $
            groupBy ((&&) `on` (==) CommandStep) commands where
            showGroup (CommandLeft:_) s' = ',':'L':s'
            showGroup (CommandRight:_) s' = ',':'R':s'
            showGroup steps s' = ',' : shows (length steps) s'

parser :: (Vector v e, Integral e, MonadParsec err String m) => m (v e)
parser = Vector.fromList <$> signed (return ()) decimal `sepBy` char ','

draw :: (Vector v Int) => v Int -> (Map (Int, Int) Int, (Int, Int), (Int, Int))
draw mem0 = (Map.fromSet crossing points, position, direction) where
    output = lines $ fmap chr $ runST $ Intcode.Vector.run mem0 []
    points = Set.fromList
        [(x, y) | (y, s) <- zip [0..] output, (x, c) <- zip [0..] s, c /= '.']
    [(position, direction)] = do
        (y, line) <- zip [0..] output
        (x, Just dir) <- zip [0..] $ lookupDirection <$> line
        return ((x, y), dir)
    lookupDirection '<' = Just (-1, 0)
    lookupDirection '>' = Just (1, 0)
    lookupDirection '^' = Just (0, -1)
    lookupDirection 'v' = Just (0, 1)
    lookupDirection _ = Nothing
    crossing (x, y) =
        if Set.size (dirs `Set.intersection` points) >= 3 then 2 else 1 where
        dirs = Set.fromList [(x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)]

paths :: (Num a, Ord a) => Map (a, a) Int -> (a, a) -> (a, a) -> [[Command]]
paths = paths' False where
    paths' False points pos dir@(dx, dy) =
        paths' True points pos dir
        ++ ((CommandLeft :) <$> paths' True points pos (dy, -dx))
        ++ ((CommandRight :) <$> paths' True points pos (-dy, dx))
    paths' True points pos@(x, y) dir@(dx, dy)
      | (Just _, points') <- Map.updateLookupWithKey (const decToZero) pos points
      = if Map.null points' then [[]] else
        (CommandStep:) <$> paths' False points' (x + dx, y + dy) dir
      | otherwise = []
    decToZero n = if n > 1 then Just $ n - 1 else Nothing

programs :: (Eq a, Show a, Show b) => Int -> ([a] -> Bool) -> [b] -> [a] -> [([b], [[a]])]
programs maxCount accept ids = map (first reverse) . programs' [] [] ids . traceShowId where
    programs' declared k _ [] = [(k, declared)]
    programs' _ k _ _ | length k >= maxCount = []
    programs' declared k [] input
      | length input < 164
      , traceShow (reverse k, map (:[]) declared, input) False = undefined
    programs' declared k ids' input =
      [ (k', declared')
      | (identifier, program) <- zip ids declared
      , Just rest <- [stripPrefix program input]
      , (k', declared') <- programs' declared (identifier:k) [] rest
      ] ++ case ids' of
        (identifier:ids'') ->
          [ (k', declared')
          | (program@(_:_), rest) <- zip (inits input) (tails input)
          , then takeWhile by accept program
          , (k', declared') <-
                programs' (declared ++ [program]) (identifier:k) ids'' rest
          ]
        _ -> []

day17a :: String -> Either (ParseErrorBundle String Void) Int
day17a input = do
    mem0 <- parse (parser @Unboxed.Vector) "" input
    let (points, _, _) = draw mem0
    return $ sum $ fmap (uncurry (*)) $ Map.keys $ Map.filter (> 1) points

day17b :: String -> Either (ParseErrorBundle String Void) Int
day17b input = do
    mem0 <- parse (parser @Unboxed.Vector) "" input
    {-
    let (points, start, direction) = draw mem0
        allPaths = paths points start direction
        (ids, programs'):_ = allPaths >>=
            programs 10 ((<= 20) . length . show) "ABC"
    traceM $ intersperse ',' ids
    mapM_ traceShowM programs'
    -}
    let intcodeInput = ord <$> unlines
          [ "A,B,C,B,A,C,A,C,A,B"
          , "L,6,R,2,R,2,L,12,R,6"
          , "L,10,R,10,R,6,L,4"
          , "R,10,L,4,L,4,L,12"
          , "n"
          ]
        getInput (i:input') = setInput (getInput input') $> i
        getInput _ = fail "no input"
        loop s = getOutput >>= \case
            Just 10 -> traceM (reverse s) >> loop ""
            Just c | c < 256 -> loop $ chr c : s
            Just x -> return x
            Nothing -> fail "unexpected termination"
    return $ runST $ do
        mem <- memory $ mem0 // [(0, 2)]
        evalIntcodeT (loop "") mem $ getInput intcodeInput
