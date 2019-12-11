{-# LANGUAGE RecordWildCards #-}
module Intcode.Vector (memory, run) where

import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad)
import Data.Functor (($>))
import Data.Primitive.MutVar (newMutVar, readMutVar, writeMutVar)
import Data.Vector.Generic (Vector, thaw)
import Data.Vector.Generic.Mutable (unsafeGrow, unsafeRead, unsafeWrite)
import qualified Data.Vector.Generic.Mutable as Vector (length)
import Intcode (Memory(..))
import qualified Intcode (run)

memory :: (Vector v e, Integral e, PrimMonad m) => v e -> m (Memory m e Int)
memory mem0 = do
    mem <- thaw mem0 >>= newMutVar
    let readMem i | i < 0 = fail "negtive index"
        readMem i = do
            mem' <- readMutVar mem
            if i < Vector.length mem'
            then unsafeRead mem' i
            else return 0
        writeMem i _ | i < 0 = fail "negative index"
        writeMem i e = do
            mem' <- readMutVar mem
            let size = Vector.length mem'
            mem'' <- if i < size then return mem' else do
                let size' = max (i + 1) (2 * size)
                mem'' <- unsafeGrow mem' $ size' - size
                forM_ [size..size' - 1] $ unsafeWrite mem'' `flip` 0
                writeMutVar mem mem'' $> mem''
            unsafeWrite mem'' i e
    return Memory {..}

run :: (Vector v e, Integral e, PrimMonad m) => v e -> [e] -> m [e]
run mem0 input = memory mem0 >>= flip Intcode.run input
