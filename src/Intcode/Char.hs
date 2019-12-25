{-# LANGUAGE LambdaCase #-}
module Intcode.Char (runAsciiTraceUntilInt) where

import Data.Char (chr, ord)
import Data.Functor (($>))
import Debug.Trace (traceM)
import Intcode (Memory, evalIntcodeT, getOutput, setInput)

runAsciiTraceUntilInt :: (Monad m) => Memory m Int -> String -> m (Maybe Int)
runAsciiTraceUntilInt mem input = evalIntcodeT (captureOutput "") mem $
    traceM input >> getInput (ord <$> input) where
    getInput (i:input') = setInput (getInput input') $> i
    getInput _ = fail "no input"
    captureOutput s = getOutput >>= \case
        Just 10 -> traceM (reverse s) >> captureOutput ""
        Just c | c < 256 -> captureOutput $ chr c : s
        Just x -> traceM (reverse s) >> return (Just x)
        Nothing -> traceM (reverse s) >> return Nothing
