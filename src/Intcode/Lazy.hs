{-# LANGUAGE FlexibleContexts, RecordWildCards, ViewPatterns #-}
module Intcode.Lazy (memory) where

import Control.Monad.State.Class (MonadState, gets, modify)
import Data.Maybe (fromMaybe)
import Data.Vector.Generic (Vector, (!?), snoc)
import qualified Data.Vector.Generic as Vector ((++), length, modify, replicate)
import Data.Vector.Generic.Mutable (unsafeWrite)
import Intcode (Memory(..))

memory :: (Vector v e, Integral e, MonadState (v e) m) => Memory m e
memory = Memory {..} where
    readMem i | i < 0 = fail "negtive index"
    readMem (fromIntegral -> i) = gets $ \mem -> fromMaybe 0 $ mem !? i
    writeMem i _ | i < 0 = fail "negative index"
    writeMem (fromIntegral -> i) e = modify $ \mem ->
        let size = Vector.length mem
        in if i < size
            then Vector.modify (\v -> unsafeWrite v i e) mem
            else mem Vector.++ Vector.replicate (i - size) 0 `snoc` e
