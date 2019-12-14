{-|
Module:         Day14
Description:    <https://adventofcode.com/2019/day/14 Day 14: Space Stoichiometry>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module Day14 (day14a, day14b) where

import Control.Monad (forM_)
import Control.Monad.Trans.Writer (execWriter, tell)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (assocs, elems, filterWithKey, findWithDefault, fromList, insert, keys, lookup, lookupMin, singleton, unionWith)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy, sepEndBy, some)
import Text.Megaparsec.Char (alphaNumChar, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Printf (printf)

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

day14b :: String -> Either (ParseErrorBundle String ()) String
day14b input = do
    rules <- parse (parser @Int) "" input
    return $ execWriter $ do
    forM_ (zip [1..] $ Map.keys rules) $
        tell . uncurry @Int (printf "var rule%d integer >= 0; # => %s\n")
    tell "s.t. ORE: 0"
    forM_ (zip [1..] $ Map.findWithDefault 0 "ORE" . snd <$> Map.elems rules) $
        tell . uncurry @Int (printf " + rule%d * (%d)")
    tell " <= 1000000000000;\n"
    let sums = [Map.insert k n $ negate <$> m | (k, (n, m)) <- Map.assocs rules]
    forM_ (Map.keys rules) $ \k -> do
        let expr = zip [1..] $ Map.findWithDefault 0 k <$> sums
        tell $
            if k == "FUEL"
            then printf "maximize %s: 0" k
            else printf "s.t. %s: 0 <= 0" k
        forM_ expr $ tell . uncurry @Int (printf " + rule%d * (%d)")
        tell ";\n"
    tell "solve;\n"
