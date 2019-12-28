{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Intcode.Diff (DiffableMemory, checkDiff, getDiffs, mem, undoDiffs, wrapMemory) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (assocs, delete, empty, insert, lookup, null)
import Data.Primitive.MutVar (MutVar, modifyMutVar', newMutVar, readMutVar, writeMutVar)
import Intcode (Memory(..))

data DiffableMemory m e = DiffableMemory
  { delegate :: Memory m e
  , diffRef :: MutVar (PrimState m) (Map e e)
  }

-- |Returns 'True' if the memory is the same as at the last 'checkDiff'.
checkDiff :: (PrimMonad m, Ord e) => DiffableMemory m e -> m Bool
checkDiff DiffableMemory {..} = do
    diff <- readMutVar diffRef <* writeMutVar diffRef Map.empty
    mapM_ (uncurry $ writeMem delegate) $ Map.assocs diff
    writeMutVar diffRef Map.empty $> Map.null diff

-- |Returns memory diffs since the last 'checkDiff'.
getDiffs :: (PrimMonad m, Ord e) => DiffableMemory m e -> m (Map e e)
getDiffs DiffableMemory {diffRef} = readMutVar diffRef

-- |Restores memory to the state at the last 'checkDiff'.
undoDiffs :: (PrimMonad m, Ord e) => DiffableMemory m e -> m ()
undoDiffs DiffableMemory {diffRef} = writeMutVar diffRef Map.empty

mem :: (PrimMonad m, Ord e) => DiffableMemory m e -> Memory m e
mem DiffableMemory {delegate = Memory {..}, ..} = Memory
  { readMem = \i ->
        Map.lookup i <$> readMutVar diffRef >>= maybe (readMem i) return
  , writeMem = \i v -> do
        v' <- readMem i
        modifyMutVar' diffRef $ if v == v' then Map.delete i else Map.insert i v
  }

wrapMemory :: (PrimMonad m, Ord e) => Memory m e -> m (DiffableMemory m e)
wrapMemory delegate = DiffableMemory delegate <$> newMutVar Map.empty
