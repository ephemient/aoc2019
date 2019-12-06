{-|
Module:         Day6
Description:    <https://adventofcode.com/2019/day/6 Day 6: Universal Orbit Map>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications, TypeFamilies #-}
module Day6 (day6a, day6b) where

import Control.Monad (ap)
import Data.List (elemIndex, nub)
import Data.Tuple (swap)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepEndBy, some)
import Text.Megaparsec.Char (alphaNumChar, char, space1)
import Data.Graph.Inductive hiding (ap)

parser :: (Graph gr, MonadParsec e String m) => m (gr String ())
parser = toGraph <$> line `sepEndBy` space1 where
    line = (,) <$> some alphaNumChar <* char ')' <*> some alphaNumChar
    toGraph edgeNames = mkGraph (zip [0..] names)
      [ (i, j, ())
      | (x, y) <- edgeNames
      , let Just i = elemIndex x names
            Just j = elemIndex y names
      ] where names = nub . uncurry (++) $ unzip edgeNames

day6a :: String -> Either (ParseErrorBundle String ()) Int
day6a input =
    sum . ap (map . numReachable) nodes <$> parse (parser @Gr) "" input where
    numReachable graph node = length (reachable node graph) - 1

day6b :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day6b input = youToSan <$> parse (parser @Gr) "" input where
    youToSan graph = do
        let names = swap <$> labNodes graph
        you <- lookup "YOU" names
        san <- lookup "SAN" names
        depth <- lookup san . level you $ undir graph
        return $ depth - 2
