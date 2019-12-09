{-# LANGUAGE RecordWildCards #-}
module Intcode.Vector where

import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Functor (($>))
import Data.Primitive.MutVar (newMutVar, readMutVar, writeMutVar)
import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as Vector (length, unsafeGrow, unsafeRead, unsafeWrite)
import Intcode (Memory(..))
import qualified Intcode (run)

run :: (MVector v e, Integral e, PrimMonad m) =>
    v (PrimState m) e -> [e] -> m [e]
run mem0 input = do
    mem <- newMutVar mem0
    let readMem i | i < 0 = fail "negtive index"
        readMem i = do
            mem' <- readMutVar mem
            if i < Vector.length mem'
            then Vector.unsafeRead mem' i
            else return 0
        writeMem i _ | i < 0 = fail "negative index"
        writeMem i e = do
            mem' <- readMutVar mem
            let size = Vector.length mem'
            mem'' <- if i < size then return mem' else do
                let size' = max (i + 1) (2 * size)
                mem'' <- Vector.unsafeGrow mem' $ size' - size
                forM_ [size..size' - 1] $ Vector.unsafeWrite mem'' `flip` 0
                writeMutVar mem mem'' $> mem''
            Vector.unsafeWrite mem'' i e
    Intcode.run Memory {..} input
