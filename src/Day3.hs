{-|
Module:         Day3
Description:    <https://adventofcode.com/2019/day/3 Day 3: Crossed Wires>
-}
{-# LANGUAGE FlexibleContexts, TupleSections, TypeApplications, TypeFamilies, ViewPatterns #-}
module Day3 (day3a, day3b) where

import Control.Arrow (first, second)
import Control.Monad (foldM, foldM_)
import Control.Monad.Writer.Strict (execWriter, tell)
import Data.Functor (($>))
import qualified Data.Map.Strict as Map (alterF, empty)
import Data.Semigroup (Semigroup(..), stimesIdempotentMonoid)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, choice, option, parse, sepEndBy)
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (Integral a, MonadParsec e String m) => m [[(a, a)]]
parser = line ((0, 0) :) (0, 0) `sepEndBy` space1 where
    dir = choice $ zipWith (($>) . char) "ULDR"
        [second succ, first pred, second pred, first succ]
    line k p = do
        f <- dir
        n <- decimal
        let points@(last -> p') = take n . tail $ iterate f p
        option (k points) $ char ',' *> line (k . (points ++)) p'

newtype MaybeMin a = MaybeMin { getMaybeMin :: Maybe a }
instance (Ord a) => Semigroup (MaybeMin a) where
    MaybeMin Nothing <> y = y
    x <> MaybeMin Nothing = x
    MaybeMin (Just x) <> MaybeMin (Just y) = MaybeMin . Just $! min x y
    stimes = stimesIdempotentMonoid
instance (Ord a) => Monoid (MaybeMin a) where
    mempty = MaybeMin Nothing

intersections :: (Monad m, Ord k) =>
    (k -> a -> Maybe b -> m (Maybe b)) -> [[(k, a)]] -> m ()
intersections f = foldM_ follow Map.empty . zip @Int [0..] where
    follow m (i, walk) = foldM follow' m walk where
        follow' m' (k, a) = Map.alterF f' k m' where
            f' Nothing = fmap (i,) <$> f k a Nothing
            f' old@(Just (j, o))
              | i == j = pure old
              | otherwise = fmap (i,) <$> f k a (Just o)

day3a :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day3a input = getMaybeMin . execWriter . intersections f .
    map (map (, ())) <$> parse parser "" input where
    f (0, 0) _ _ = pure Nothing
    f (manhattan -> d) _ Nothing = pure $ Just d
    f (manhattan -> d) _ (Just o) =
        let new = Just $! min d o in tell (MaybeMin new) $> new
    manhattan (x, y) = abs x + abs y

day3b :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day3b input = getMaybeMin . execWriter . intersections f .
    map (flip zip [0..]) <$> parse (parser @Int) "" input where
    f (0, 0) _ _ = pure Nothing
    f _ d Nothing = pure $ Just d
    f _ d (Just o) = tell (MaybeMin . Just $! d + o) $> (Just $! min d o)
