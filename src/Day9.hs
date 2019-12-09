{-|
Module:         Day9
Description:    <https://adventofcode.com/2019/day/9 Day 9: Sensor Boost>
-}
{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module Day9 (day9a, day9b) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Functor (($>))
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (last)
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as Vector (fromList, thaw)
import qualified Data.Vector.Unboxed.Mutable as Vector (length, unsafeGrow, unsafeRead, unsafeWrite)
import Intcode (Memory(..), run)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (Integral e, Unbox e, MonadParsec err String m) => m (Vector e)
parser = Vector.fromList <$>
    (signed (return ()) decimal `sepBy` char ',' <* space)

run' :: (Integral e, Unbox e) => Vector e -> [e] -> [e]
run' mem0 input = runST $ do
    mem <- Vector.thaw mem0 >>= newSTRef
    base <- newSTRef 0
    let readMem i | i < 0 = fail "negtive index"
        readMem i = do
            mem' <- readSTRef mem
            if i < Vector.length mem'
            then Vector.unsafeRead mem' i
            else return 0
        writeMem i _ | i < 0 = fail "negative index"
        writeMem i e = do
            mem' <- readSTRef mem
            let size = Vector.length mem'
            mem'' <- if i < size then return mem' else do
                mem'' <- Vector.unsafeGrow mem' $ i + 1 - size
                forM_ [size..i - 1] $ Vector.unsafeWrite mem'' `flip` 0
                writeSTRef mem mem'' $> mem''
            Vector.unsafeWrite mem'' i e
        readBase = readSTRef base
        modifyBase = modifySTRef' base
    run Memory {..} input

day9a :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day9a = fmap (fmap NonEmpty.last . nonEmpty . flip run' [1]) .  parse parser ""

day9b :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day9b = fmap (fmap NonEmpty.last . nonEmpty . flip run' [2]) .  parse parser ""
