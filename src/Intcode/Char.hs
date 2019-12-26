{-# LANGUAGE LambdaCase #-}
module Intcode.Char (getOutputLine, makeStringInput, runAsciiTraceUntilInt) where

import Control.Monad (mfilter)
import Data.Char (chr, ord)
import Data.Functor (($>))
import Debug.Trace (traceM)
import Intcode (IntcodeT, Memory, evalIntcodeT, getOutput, setInput)

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

getOutputLine :: (Monad m) => IntcodeT Int m (Maybe String)
getOutputLine = getOutputLine' "" where
    getOutputLine' s = getOutput >>= \case
        Just c@10 -> return . Just . reverse $ chr c : s
        Just c -> getOutputLine' $ chr c : s
        _ -> return $ reverse <$> mfilter (not . null) (Just s)

makeStringInput :: (Monad m) =>
    IntcodeT Int m Int -> String -> IntcodeT Int m Int
makeStringInput eof = makeStringInput' where
    makeStringInput' (c:s) = setInput (makeStringInput' s) $> ord c
    makeStringInput' _ = eof
