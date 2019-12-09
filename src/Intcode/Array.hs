{-# LANGUAGE RecordWildCards #-}
module Intcode.Array where

import Data.Array.MArray (Ix, MArray, readArray, writeArray)
import Intcode (Memory(..))
import qualified Intcode (run)

run :: (MArray a e m, Integral e, Ix i, Num i, Monad m) => a i e -> [e] -> m [e]
run mem input = Intcode.run Memory {..} input where
    readMem = readArray mem
    writeMem = writeArray mem
