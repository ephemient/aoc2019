{-|
Module:         Day7
Description:    <https://adventofcode.com/2019/day/7 Day 7: Amplification Circuit>
-}
{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, RecordWildCards, ScopedTypeVariables, TypeApplications #-}
module Day7 (day7a, day7b, maxAmplify) where

import Control.Monad.ST (ST, runST)
import Data.Array.ST (MArray, STArray, newListArray, readArray, writeArray)
import Data.Array.Unboxed  (IArray, UArray, bounds, elems, listArray)
import Data.Ix (Ix)
import Data.List (permutations)
import qualified Data.List.NonEmpty as NonEmpty (last)
import Data.Maybe (mapMaybe)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map.Lazy as Map (foldlWithKey, singleton)
import Intcode (Memory(..), run)
import Linear (Linear(..))
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (signed, decimal)

parser :: (IArray a e, Integral e, MonadParsec err String m) => m (a Int e)
parser = do
    ints <- signed (return ()) decimal `sepBy` char ',' <* space
    return $ listArray (0, length ints - 1) ints

maxAmplify :: forall a b e i m. (MArray a (Linear e Int) m, IArray b e, Integral e, Ix i, Num i, Monad m) =>
    b i e -> [e] -> m (Maybe e)
maxAmplify mem phases = do
    let vars = Linear @e 0 . flip Map.singleton 1 <$> [0..]
        apply input Linear {..} =
            Map.foldlWithKey expand (fromIntegral constant) variables where
            expand c k b = c + fromIntegral b * input !! k
        evaluate = foldl (map . apply) $ 0 : vars
        resolve list = solution where solution = map (apply solution) list
        run' phase = do
            mem' <- newListArray @a @(Linear e Int) (bounds mem) $
                fromIntegral <$> elems mem
            run Memory { readMem = readArray mem', writeMem = writeArray mem' } $
                fromIntegral phase : vars
    amplifiers <- mapM run' phases
    let outputs = resolve . evaluate <$> permutations amplifiers
    return . fmap maximum . nonEmpty $ NonEmpty.last <$> mapMaybe nonEmpty outputs

day7a :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day7a input = do
    mem <- parse (parser @UArray) "" input
    let maxAmplify' :: forall s. [Int] -> ST s (Maybe Int)
        maxAmplify' = maxAmplify @(STArray s) mem
    return $ runST $ maxAmplify' [0..4]

day7b :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day7b input = do
    mem <- parse (parser @UArray) "" input
    let maxAmplify' :: forall s. [Int] -> ST s (Maybe Int)
        maxAmplify' = maxAmplify @(STArray s) mem
    return $ runST $ maxAmplify' [5..9]
