{-|
Module:         Day3
Description:    <https://adventofcode.com/2019/day/3 Day 3: Crossed Wires>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, TupleSections, TypeApplications, TypeFamilies, ViewPatterns #-}
module Day3 (day3a, day3b) where

import Control.Arrow (first, second)
import Control.Monad.Writer.Strict (MonadTrans, execWriterT, lift, tell)
import Control.Monad.ST (ST, runST)
import qualified Data.HashTable.ST.Basic as HT (delete, insert, lookup, new)
import Data.Functor (($>))
import Data.Hashable (Hashable)
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

newtype Min a = Min { getMin :: Maybe a }
instance (Ord a) => Semigroup (Min a) where
    Min Nothing <> y = y
    x <> Min Nothing = x
    Min (Just x) <> Min (Just y) = Min $! Just $! min x y
    stimes = stimesIdempotentMonoid
instance (Ord a) => Monoid (Min a) where mempty = Min Nothing

intersections :: (MonadTrans t, Monad (t (ST s)), Hashable k, Eq k) =>
    (k -> a -> Maybe b -> t (ST s) (Maybe b)) -> [[(k, a)]] -> t (ST s) ()
intersections f walks = do
    field <- lift HT.new
    let follow (i, (k, a)) = lift (HT.lookup field k) >>= \case
            Just (j, _) | i == j -> return ()
            old -> do
                new <- fmap (i,) <$> f k a (snd <$> old)
                lift $ maybe (HT.delete field k) (HT.insert field k) new
    mapM_ follow (concat $ zipWith @Int (zip . repeat) [0..] walks)

day3a :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day3a = fmap go . parse parser "" where
    go x = getMin $ runST $ execWriterT $ intersections f $ map (, ()) <$> x
    f (0, 0) _ _ = pure Nothing
    f (manhattan -> d) _ Nothing = pure $ Just d
    f (manhattan -> d) _ (Just o) =
        let new = Just $! min d o in tell (Min $! new) $> new
    manhattan (x, y) = abs x + abs y

day3b :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day3b = fmap go . parse (parser @Int) "" where
    go x = getMin $ runST $ execWriterT $ intersections f $ flip zip [0..] <$> x
    f (0, 0) _ _ = pure Nothing
    f _ d Nothing = pure $ Just d
    f _ d (Just o) = tell (Min $! Just $! d + o) $> (Just $! min d o)
