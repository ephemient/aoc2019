{-|
Module:         Day14
Description:    <https://adventofcode.com/2019/day/14 Day 14: Space Stoichiometry>
-}
{-# LANGUAGE FlexibleContexts #-}
module Day14 (day14a) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (filterWithKey, findWithDefault, fromList, insert, lookup, lookupMin, singleton, unionWith)
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

day14a :: String -> Either (ParseErrorBundle String ()) Int
day14a input = do
    rules <- parse parser "" input
    let expand queue
          | Just (ele, n) <- Map.lookupMin $ Map.filterWithKey needed queue
          , Just (m, srcs) <- Map.lookup ele rules
          , x <- (n + m - 1) `div` m
          = expand $ Map.unionWith (+) queue $
                Map.insert ele (negate $ m * x) $ fmap (x *) srcs
          | otherwise = queue
        needed "ORE" _ = False
        needed _ n = n > 0
    return $ Map.findWithDefault 0 "ORE" $ expand $ Map.singleton "FUEL" 1
