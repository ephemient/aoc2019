{-|
Module:         Day20
Description:    <https://adventofcode.com/2019/day/20 Day 20: Donut Maze>
-}
{-# LANGUAGE TupleSections, ViewPatterns #-}
module Day20 (day20a, day20b) where

import Common (bfsM, neighbors)
import Control.Monad (guard)
import Control.Monad.Cont (callCC, runCont)
import Data.Char (isAlpha)
import Data.Functor (($>))
import Data.List (elemIndices, tails, transpose)
import Data.Map.Lazy (Map, (!), (!?))
import qualified Data.Map.Lazy as Map (assocs, fromList, fromListWith, size)
import Data.Maybe (catMaybes, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, member, toList)
import Data.Tuple (swap)

parse :: String -> (Set (Int, Int), Map (Int, Int) String)
parse (lines -> rows@(transpose -> cols)) = (maze, portals) where
    maze = Set.fromList
        [(x, y) | (y, row) <- zip [0..] rows, x <- elemIndices '.' row]
    match n (a:b:'.':_) | isAlpha a, isAlpha b = Just (n + 2, [a, b])
    match n ('.':a:b:_) | isAlpha a, isAlpha b = Just (n, [a, b])
    match _ _ = Nothing
    matchAll = catMaybes . zipWith match [0..] . tails
    portals = Map.fromList $
        [((x, y), s) | (y, row) <- zip [0..] rows, (x, s) <- matchAll row] ++
        [((x, y), s) | (x, col) <- zip [0..] cols, (y, s) <- matchAll col]

invertMap :: (Ord a, Ord b, Applicative t, Monoid (t a)) =>
    Map a b -> Map b (t a)
invertMap = Map.fromListWith (<>) . map (fmap pure . swap) . Map.assocs

day20a :: String -> Maybe Int
day20a input = flip runCont id $ callCC $ \exit ->
    bfsM id (go $ exit . Just) start $> Nothing where
    (maze, portals) = parse input
    reversePortals = invertMap portals
    [start] = reversePortals ! "AA"
    [end] = reversePortals ! "ZZ"
    go exit d p | p == end = exit d
    go _ _ p = return $ filter (`Set.member` maze) (neighbors p) ++ do
        portal <- maybeToList $ portals !? p
        filter (/= p) $ reversePortals ! portal

day20b :: String -> Maybe Int
day20b input = flip runCont id $ callCC $ \exit ->
    bfsM id (go $ exit . Just) (start, 0 :: Int) $> Nothing where
    (maze, portals) = parse input
    (xs, ys) = unzip $ Set.toList maze
    (minX, maxX) = (minimum xs, maximum xs)
    (minY, maxY) = (minimum ys, maximum ys)
    reversePortals = invertMap portals
    [start] = reversePortals ! "AA"
    [end] = reversePortals ! "ZZ"
    go exit d (p, 0) | p == end = exit d
    go _ _ (p@(x, y), n) = return $
        map (, n) (filter (`Set.member` maze) $ neighbors p) ++ do
            portal <- maybeToList $ portals !? p
            q <- filter (/= p) $ reversePortals ! portal
            if x `elem` [minX, maxX] || y `elem` [minY, maxY]
            then guard (n > 0) $> (q, n - 1)
            else guard (2 * n < Map.size portals) $> (q, n + 1)
