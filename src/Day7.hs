{-|
Module:         Day7
Description:    <https://adventofcode.com/2019/day/7 Day 7: Amplification Circuit>
-}
{-# LANGUAGE FlexibleContexts, RecordWildCards, TypeApplications, ViewPatterns #-}
module Day7 (day7a, day7b, maxAmplify) where

import Control.Monad.ST (runST)
import Data.List (permutations)
import qualified Data.List.NonEmpty as NonEmpty (last)
import Data.Maybe (mapMaybe)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map.Lazy as Map (empty, foldlWithKey, singleton)
import qualified Data.Vector as Boxed (Vector)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as Vector (convert, fromList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Data.Void (Void)
import Intcode (run)
import Intcode.Vector (memory)
import Linear (Linear(..))
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (signed, decimal)

parser :: (Vector v e, Integral e, MonadParsec err String m) => m (v e)
parser = Vector.fromList <$> signed (return ()) decimal `sepBy` char ','

maxAmplify :: (Vector v e, Integral e) => v e -> [e] -> Maybe e
maxAmplify (fmap (`Linear` Map.empty) . Vector.convert -> mem0) phases =
    fmap maximum . nonEmpty $ NonEmpty.last <$> mapMaybe nonEmpty outputs where
    vars = Linear 0 . flip Map.singleton 1 <$> [0..]
    apply input Linear {..} =
        Map.foldlWithKey expand (fromIntegral constant) variables where
        expand c k b = c + fromIntegral b * input !! k
    evaluate = foldl (map . apply) $ 0 : vars
    resolve list = solution where solution = map (apply solution) list
    run' phase = runST $
        memory @Boxed.Vector mem0 >>= flip run (fromIntegral phase : vars)
    amplifiers = run' <$> phases
    outputs = resolve . evaluate <$> permutations amplifiers

day7a :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day7a input = flip maxAmplify [0..4] <$> parse (parser @Unboxed.Vector) "" input

day7b :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day7b input = flip maxAmplify [5..9] <$> parse (parser @Unboxed.Vector) "" input
