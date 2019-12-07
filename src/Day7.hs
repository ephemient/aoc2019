{-|
Module:         Day7
Description:    <https://adventofcode.com/2019/day/7 Day 7: Amplification Circuit>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications, ViewPatterns #-}
module Day7 (amplify, day7a, day7b) where

import Control.Monad.State (evalState, gets, modify)
import Data.Array.Unboxed (IArray, Ix, UArray, (!), (//), listArray)
import Data.List (foldl', permutations)
import qualified Data.List.NonEmpty as NonEmpty (last)
import Data.List.NonEmpty (nonEmpty)
import Intcode (Memory(..), run)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (signed, decimal)

parser :: (IArray a e, Integral e, MonadParsec err String m) => m (a Int e)
parser = do
    ints <- signed (return ()) decimal `sepBy` char ',' <* space
    return $ listArray (0, length ints - 1) ints

amplify :: (IArray a e, Integral e, Ix i, Num i) => a i e -> [e] -> Maybe e
amplify mem (o:os) = NonEmpty.last <$> nonEmpty final where
    run' inject input = flip evalState mem $ do
        let memory = Memory
              { readMem = \n -> gets (! n)
              , writeMem = \n v -> modify (// [(n, v)])
              }
        run memory $ inject ++ input
    injects = [o, 0] : map (:[]) os
    amps@(last -> final) = zipWith run' injects $ final : amps
amplify _ _ = Nothing

day7a :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day7a input = do
    mem <- parse (parser @UArray) "" input
    return $ foldl' max Nothing $ amplify mem <$> permutations [0..4]

day7b :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day7b input = do
    mem <- parse (parser @UArray) "" input
    return $ foldl' max Nothing $ amplify mem <$> permutations [5..9]
