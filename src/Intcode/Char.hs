{-# LANGUAGE LambdaCase #-}
module Intcode.Char (runTraced) where

import Data.Char (chr, ord)
import Data.Functor (($>))
import Debug.Trace (traceM)
import Intcode (Memory, evalIntcodeT, getOutput, setInput)

runTraced :: (Monad m) => Memory m Int -> [Char] -> m [Int]
runTraced mem input = evalIntcodeT (captureOutput [] "") mem $
    traceM input >> getInput (ord <$> input) where
    getInput (i:input') = setInput (getInput input') $> i
    getInput _ = fail "no input"
    captureOutput xs s = getOutput >>= \case
        Just 10 -> traceM (reverse s) >> captureOutput xs ""
        Just c | c < 256 -> captureOutput xs $ chr c : s
        Just x -> captureOutput (x:xs) s
        Nothing -> return $ reverse xs
