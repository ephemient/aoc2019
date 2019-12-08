{-|
Module:         Day8
Description:    <https://adventofcode.com/2019/day/8 Day 8: Space Image Format>
-}
module Day8 (day8a, day8b) where

import Data.List (intercalate, minimumBy, transpose)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)
import Data.Semigroup (Semigroup(stimes), stimesIdempotentMonoid)

data Color = Black | White | Transparent deriving (Eq)
instance Semigroup Color where
    Transparent <> y = y
    x <> _ = x
    stimes = stimesIdempotentMonoid
instance Monoid Color where
    mempty = Transparent
instance Show Color where
    showsPrec _ Transparent = ('\x2591':)
    showsPrec _ Black = ('\x2592':)
    showsPrec _ White = ('\x2593':)
    showList = foldr ((.) . shows) id

parse :: Char -> Maybe Color
parse '0' = Just Black
parse '1' = Just White
parse '2' = Just Transparent
parse _ = Nothing

count :: (Eq a) => a -> [a] -> Int
count = (.) length . filter . (==)

day8a :: Int -> Int -> String -> Int
day8a width height input = count White chunk * count Transparent chunk where
    chunk = minimumBy (comparing $ count Black) . chunksOf (width * height) $
        mapMaybe parse input

day8b :: Int -> Int -> String -> String
day8b width height = intercalate "\n" . map show . chunksOf width .
    map mconcat . transpose . chunksOf (width * height) . mapMaybe parse
