{-|
Module:         Day23
Description:    <https://adventofcode.com/2019/day/23 Day 23: Category Six>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, NamedFieldPuns, RecordWildCards, TupleSections, TypeApplications, ViewPatterns #-}
module Day23 (day23a, day23b) where

import Control.Arrow ((>>>))
import Control.Monad (forM)
import Control.Monad.Cont (callCC, runContT)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.ST (runST)
import Control.Monad.Trans (lift)
import Data.Functor (($>))
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (last)
import Data.List.Split (chunksOf)
import Data.Map.Lazy ((!?))
import qualified Data.Map as Map (findWithDefault, fromListWith)
import Data.Primitive.MutVar (modifyMutVar, newMutVar, readMutVar, writeMutVar)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as Vector (fromList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Data.Void (Void)
import Intcode (Memory, State(..), getOutput, getState, liftMemory, runIntcodeT, setInput)
import Intcode.Diff (checkDiff, mem, wrapMemory)
import Intcode.Vector (memory)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (Vector v e, Integral e, MonadParsec err String m) => m (v e)
parser = Vector.fromList <$> signed (return ()) decimal `sepBy` char ','

newtype Computer m e =
    Computer { runComputer :: [e] -> m ([e], Maybe (Computer m e)) }

newComputer :: (Integral e, PrimMonad m) => Memory m e -> m (Computer m e)
newComputer delegate = do
    diffable <- wrapMemory delegate
    let mem' = liftMemory $ mem diffable
        go state inputs = do
            lastInput <- newMutVar False
            outputsRef <- newMutVar []
            flip runContT (save outputsRef) $ callCC $ \exit ->
                runIntcodeT (collectOutputs lastInput outputsRef) mem'
                    state {input = getInput exit lastInput inputs}
        collectOutputs lastInput outputsRef = getOutput >>= \case
            Nothing -> return False
            Just output -> do
                lift . lift $ writeMutVar lastInput False
                lift . lift $ modifyMutVar outputsRef (output:)
                collectOutputs lastInput outputsRef
        getInput exit lastInput (i:inputs) =
            setInput (getInput exit lastInput inputs) $> i
        getInput exit lastInput _ = do
            isLoop <- lift . lift $
                (&&) <$> readMutVar lastInput <*> checkDiff diffable
            if isLoop
            then getState >>= lift . exit . (, True)
            else lift (lift $ writeMutVar lastInput True) $> (-1)
        save outputsRef (state, runnable) = do
            outputs <- readMutVar outputsRef
            let computer
                  | runnable = Just . Computer $ go state
                  | otherwise = Nothing
            return (reverse outputs, computer)
    return . Computer $ go State {input = undefined, base = 0, ip = 0}

data RunState m e =
    RunState { pendingInput :: [e], next :: Maybe (Computer m e) }

newtype NAT m e a = NAT { runNAT :: (NAT m e a -> [e] -> m a) -> [e] -> m a }

day23 :: (Vector v e, Integral e, PrimMonad m) => NAT m e a -> Int -> v e -> m a
day23 nat count mem0 = do
    computers <- forM [0..count - 1] $ fromIntegral >>> \i -> do
        computer <- memory mem0 >>= newComputer
        return RunState { pendingInput = [i], next = Just computer }
    monitor [] nat computers
  where
    monitor prev nat' computers
      | (pre, RunState {next = ~(Just computer), ..} : post) <-
            span (null . pendingInput) computers
      = do
            (output, next) <- runComputer computer pendingInput
            let sends = Map.fromListWith (++)
                    [(n, xs) | n:xs <- chunksOf 3 output]
                appendInput n runState@RunState {pendingInput = pendingInput'}
                  | Just newInput <- sends !? n
                  = runState {pendingInput = pendingInput' ++ newInput}
                  | otherwise = runState
                prev' = prev ++ Map.findWithDefault [] 255 sends
            monitor prev' nat' . zipWith appendInput [0..] $
                pre ++ RunState {pendingInput = [], ..} : post
      | ~(computer0@RunState {pendingInput} : post) <- computers
      = let resume nat'' input = monitor [] nat'' $
                computer0 {pendingInput = pendingInput ++ input} : post
        in runNAT nat' resume prev

day23a :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day23a input = do
    mem0 <- parse @Void (parser @Unboxed.Vector @Int) "" input
    let nat _ (_:y:_) = return $ Just y
        nat _ _ = return Nothing
    return $ runST $ day23 (NAT nat) 50 mem0

day23b :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day23b input = do
    mem0 <- parse @Void (parser @Unboxed.Vector @Int) "" input
    let nat prev@(Just y) _ (fmap NonEmpty.last . nonEmpty -> Just y')
          | y == y' = return prev
        nat _ resume (reverse -> y:x:_) = resume (NAT . nat $ Just y) [x, y]
        nat _ _ _ = return Nothing
    return $ runST $ day23 (NAT $ nat Nothing) 50 mem0
