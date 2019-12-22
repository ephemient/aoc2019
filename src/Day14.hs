{-|
Module:         Day14
Description:    <https://adventofcode.com/2019/day/14 Day 14: Space Stoichiometry>
-}
{-# LANGUAGE FlexibleContexts, NumericUnderscores #-}
module Day14 (day14a, day14b, day14c) where

import Control.Monad.Loops (whileJust_)
import Control.Monad.RWS (MonadReader, MonadState, MonadWriter, asks, evalRWS, gets, put, tell)
import Data.Functor (($>))
import Data.List (foldl')
import Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map (delete, filterWithKey, findWithDefault, fromList, insert, lookupMin, minViewWithKey, singleton, unionWith)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy, sepEndBy, some)
import Text.Megaparsec.Char (alphaNumChar, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (Integral a, MonadParsec e String m) =>
    m (Map String (a, Map String a))
parser = Map.fromList <$> line `sepEndBy` newline where
    line = do
        srcs <- item `sepBy` string ", "
        (ele, n) <- string " => " *> item
        return (ele, (n, Map.fromList srcs))
    item = flip (,) <$> (decimal <* space) <*> some alphaNumChar

produce :: (Integral a) => Map String (a, Map String a) -> a -> a
produce rules = expand . Map.singleton "FUEL" where
    expand queue
      | Just (ele, n) <- Map.lookupMin $ Map.filterWithKey needed queue
      , Just (m, srcs) <- rules !? ele
      , x <- (n + m - 1) `div` m
      = expand $ Map.unionWith (+) queue $
            Map.insert ele (negate $ m * x) $ fmap (x *) srcs
      | otherwise = Map.findWithDefault 0 "ORE" queue
    needed "ORE" _ = False
    needed _ n = n > 0

day14a :: String -> Either (ParseErrorBundle String Void) Int
day14a = fmap (`produce` 1) . parse parser ""

day14b :: String -> Either (ParseErrorBundle String Void) Int
day14b input = do
    rules <- parse parser "" input
    let target = 1000000000000
        loop good bad
          | maybe True (good + 1 <) bad =
                case produce rules mid `compare` target of
                    LT -> loop mid bad
                    EQ -> mid
                    GT -> loop good $ Just mid
          | otherwise = good where
            mid = maybe (2 * good) ((`div` 2) . (good +)) bad
        base = produce rules 1
    return $ loop (target `div` base) Nothing

(+:) :: (Integral a) => (a, a) -> (a, a) -> (a, a)
(a, b) +: (c, d) = (a * (e `div` b) + c * (e `div` d), e) where e = lcm b d

produceFully :: (Integral a, Monad m, MonadReader (Map String (a, Map String a)) m, MonadState (Map String (a, a)) m, MonadWriter [(a, a)] m) =>
    m ()
produceFully = whileJust_ (gets Map.minViewWithKey) expand where
    expand ((ele, (n, d)), state) = do
        (m, srcs) <- asks (! ele)
        let a = gcd n m
            n' = n `div` a
            d' = m `div` a * d
            mul x = (n' * x, d')
        srcs' <- case srcs !? "ORE" of
            Just x -> tell [mul x] $> Map.delete "ORE" srcs
            _ -> return srcs
        put . Map.unionWith (+:) state $ mul <$> srcs'

day14c :: String -> Either (ParseErrorBundle String Void) (Integer, Integer)
day14c input = do
    rules <- parse parser "" input
    let (_, reqs) = evalRWS produceFully rules $ Map.singleton "FUEL" (1, 1)
    return $ foldl' (+:) (0, 1) reqs
