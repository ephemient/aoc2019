{-|
Module:         Day15
Description:    <https://adventofcode.com/2019/day/15 Day 15: Oxygen System>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, TupleSections, TypeApplications, ViewPatterns #-}
module Day15 (day15a, day15b) where

import Control.Monad.ST (runST)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map (assocs, fromList, keysSet, mapWithKey, restrictKeys)
import Data.Set (Set, (\\))
import qualified Data.Set as Set (empty, insert, intersection, member, minView,null, singleton, toList, union, unions)
import Data.Sequence (Seq(..), (><), (|>))
import qualified Data.Sequence as Seq (fromList, singleton)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as Vector (fromList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Data.Void (Void)
import Intcode (IntcodeT, evalIntcodeT, getOutput, setInput)
import Intcode.Vector (memory)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (Vector v e, Integral e, MonadParsec err String m) => m (v e)
parser = Vector.fromList <$> signed (return ()) decimal `sepBy` char ','

neighbors :: (Num e, Num a, Ord a) => (a, a) -> Map (a, a) e
neighbors (x, y) = Map.fromList
  [ ((x, y - 1), 1) -- north
  , ((x, y + 1), 2) -- south
  , ((x - 1, y), 3) -- west
  , ((x + 1, y), 4) -- east
  ]

findPath :: (Num e, Num a, Ord a) =>
    Set (a, a) -> (a, a) -> (a, a) -> Seq ((a, a), e)
findPath pass start end = findPath' pass $ Seq.singleton (start, Empty) where
    findPath' pass' ((pos, path) :<| queue)
      | pos == end = path
      | otherwise = findPath' (pass' \\ Map.keysSet next) $
            queue >< Seq.fromList (Map.assocs $ (path |>) <$> next) where
            next = Map.mapWithKey (,) (neighbors pos) `Map.restrictKeys` pass'
    findPath' _ _ = error "no path"

move :: (Monad m, Integral e) => e -> IntcodeT e m (Maybe e)
move direction = do
    setInput $ return direction
    getOutput

navigate :: (Monad m, Num a, Ord a, Integral e) =>
    Set (a, a) -> (a, a) -> (a, a) -> IntcodeT e m ((a, a), Maybe e)
navigate pass start end =
    navigate' (start, Nothing) $ findPath (Set.insert end pass) start end where
    navigate' (pos, _) ((pos', direction) :<| rest) = do
        result' <- move direction
        case result' of
            Just 1 -> navigate' (pos', result') rest
            Just 2 -> navigate' (pos', result') rest
            _ -> return (pos, result')
    navigate' lastResult _ = return lastResult

explore :: (Num a, Ord a, Integral e, Monad m) =>
    IntcodeT e m (Maybe ((Set (a, a), (a, a))))
explore = explore' (0, 0) Set.empty (Set.singleton (0, 0)) Nothing $
    neighbors' (0, 0) where
    explore' pos wall pass oxy (Set.minView -> Just (target, queue))
      | target `Set.member` wall || target `Set.member` pass
      = explore' pos wall pass oxy queue
      | otherwise = navigate pass pos target >>= \case
            (pos', Just 0) ->
                explore' pos' (Set.insert target wall) pass oxy queue
            (pos', Just 1) ->
                explore' pos' wall (Set.insert pos' pass) oxy $
                    Set.union queue $ neighbors' pos'
            (pos', Just 2) | Nothing <- oxy ->
                explore' pos' wall (Set.insert pos' pass) (Just pos') $
                    Set.union queue $ neighbors' pos'
            _ -> return Nothing
    explore' _ _ pass (Just oxy) _ = return $ Just (pass, oxy)
    explore' _ _ _ _ _ = return Nothing
    neighbors' = Map.keysSet . neighbors @Int

flood :: (Num a, Ord a) => Set (a, a) -> (a, a) -> [Set (a, a)]
flood pass = takeWhile (not . Set.null) . fmap snd . iterate flood' .
    (pass,) . Set.singleton where
    grow = Set.unions . fmap (Map.keysSet . neighbors @Int) . Set.toList
    flood' (pass', points) =
        (pass' \\ points, Set.intersection pass' $ grow points \\ points)

day15a :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day15a input = do
    mem0 <- parse (parser @Unboxed.Vector @Int) "" input
    return $ fmap (length . ($ (0, 0)) . uncurry (findPath @Int)) $ runST $ do
        mem <- memory mem0
        evalIntcodeT (explore @Int) mem $ return 0

day15b :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day15b input = do
    mem0 <- parse (parser @Unboxed.Vector @Int) "" input
    return $ fmap (length . drop 1 . uncurry flood) $ runST $ do
        mem <- memory mem0
        evalIntcodeT (explore @Int) mem $ return 0
