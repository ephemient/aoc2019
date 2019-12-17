{-|
Module:         Day17
Description:    <https://adventofcode.com/2019/day/17 Day 17: Set and Forget>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module Day17 (day17a) where

import Control.Monad.ST (runST)
import Data.Char (chr)
import qualified Data.Set as Set (elems, filter, fromList, intersection, size)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as Vector (fromList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Data.Void (Void)
import qualified Intcode.Vector (run)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (Vector v e, Integral e, MonadParsec err String m) => m (v e)
parser = Vector.fromList <$> signed (return ()) decimal `sepBy` char ','

day17a :: String -> Either (ParseErrorBundle String Void) Int
day17a input = do
    mem0 <- parse (parser @Unboxed.Vector @Int) "" input
    let ret = lines $ fmap chr $ runST $ Intcode.Vector.run mem0 []
        scaf = Set.fromList $ do
            (y, line) <- zip [0..] ret
            (x, '#') <- zip [0..] line
            return (x, y)
        int (x, y) = Set.size (Set.intersection scaf dirs) >= 3 where
            dirs = Set.fromList [(x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)]
    return $ sum $ map (uncurry (*)) $ Set.elems $ Set.filter int scaf
