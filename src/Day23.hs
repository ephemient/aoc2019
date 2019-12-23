{-|
Module:         Day23
Description:    <https://adventofcode.com/2019/day/23 Day 23: Category Six>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, TypeApplications #-}
module Day23 (day23a, day23b) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, takeMVar, tryPutMVar, tryTakeMVar)
import Control.Concurrent.QSemN (QSemN, newQSemN, signalQSemN, waitQSemN)
import Control.Monad (forM_, replicateM)
import Control.Monad.Fix (fix)
import Control.Monad.Trans (lift)
import Data.Functor (($>), void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (delete, empty, insert, toList)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as Vector (fromList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Data.Void (Void)
import Debug.Trace (traceM)
import Intcode (IntcodeT, evalIntcodeT, getOutput, setInput)
import Intcode.Vector (memory)
import System.Timeout (timeout)
import Text.Megaparsec (MonadParsec, parse, sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Printf (printf)

parser :: (Vector v e, Integral e, MonadParsec err String m) => m (v e)
parser = Vector.fromList <$> signed (return ()) decimal `sepBy` char ','

day23a :: String -> IO Int
day23a input = do
    let Right mem0 = parse @Void (parser @Unboxed.Vector @Int) "" input
    channels <- replicateM 50 $ newMVar []
    output <- newEmptyMVar
    let ioLoop i = fix $ \self -> do
            Just n <- getOutput
            Just x <- getOutput
            Just y <- getOutput
            traceM $ printf "%d -> %d (%d, %d)" i n x y
            lift $ if n == 255
                then putMVar output (x, y)
                else modifyMVar_ (channels !! n) $ return . (++ [(x, y)])
            self
        getInput i chan = fix $ \self ->
            lift (modifyMVar chan takeHead) >>= \case
                Nothing -> traceM (printf "%d <- no input" i) $> (-1)
                Just (x, y) -> do
                    traceM $ printf "%d <- (%d, %d)" i x y
                    setInput (setInput self $> y) $> x
        takeHead (i:is) = return (is, Just i)
        takeHead _ = return ([], Nothing)
    forM_ (zip [0..] channels) $ \(i, chan) -> forkIO $ do
        mem <- memory mem0
        evalIntcodeT (ioLoop i) mem $ setInput (getInput i chan) $> i
    snd <$> takeMVar output

newIOLoop :: (Vector v Int) =>
    v Int -> [MVar [(Int, Int)]] -> MVar (Int, Int) -> [MVar ()] -> QSemN -> MVar (Set Int) -> Int -> IO ()
newIOLoop mem0 channels output unblocks sem blocked i = do
    mem <- memory mem0
    waiting <- newIORef 0 :: IO (IORef Int)
    let channel = channels !! i
        unblock = unblocks !! i
        zero = do
            lift $ writeIORef waiting 0
            lift $ void $ tryPutMVar unblock ()
            lift $ modifyMVar_ blocked $ return . Set.delete i
        ioLoop :: IntcodeT Int IO ()
        ioLoop = do
            Just n <- getOutput <* zero
            Just x <- getOutput <* zero
            Just y <- getOutput <* zero
            traceM $ printf "%d -> %d (%d, %d)" i n x y
            if n == 255
            then lift $ putMVar output (x, y)
            else do
                lift $ modifyMVar_ (channels !! n) $ return . (++ [(x, y)])
                lift $ void $ tryPutMVar (unblocks !! n) ()
            ioLoop
        getInput :: IntcodeT Int IO Int
        getInput = lift (modifyMVar channel takeHead) >>= \case
            Nothing -> do
                n <- lift $ readIORef waiting
                traceM $ printf "%d <- no input #%d" i n
                if n < 10
                then do
                    lift $ writeIORef waiting $! n + 1
                    return (-1)
                else do
                    s <- lift $ modifyMVar blocked $ \s ->
                        let s' = Set.insert i s in return (s', Set.toList s')
                    traceM $ printf "%i is blocked (%s)" i $ show s
                    lift $ void $ tryTakeMVar unblock
                    lift $ signalQSemN sem 1
                    lift $ takeMVar unblock
                    lift $ waitQSemN sem 1
                    getInput
            Just (x, y) -> do
                traceM $ printf "%d <- (%d, %d)" i x y
                zero
                setInput (setInput getInput $> y) $> x
    evalIntcodeT ioLoop mem $ setInput getInput $> i
  where
    takeHead (cur:rest) = return (rest, Just cur)
    takeHead _ = return ([], Nothing)

day23b :: String -> IO Int
day23b input = do
    let Right mem0 = parse @Void (parser @Unboxed.Vector @Int) "" input
    channels@(channel0:_) <- replicateM 50 $ newMVar []
    output <- newEmptyMVar
    unblocks@(unblock0:_) <- replicateM 50 $ newMVar ()
    sem <- newQSemN 1
    blocked <- newMVar Set.empty
    mapM_ (forkIO . newIOLoop mem0 channels output unblocks sem blocked) [0..49]
    let monitor prev = takeMVar output >>= monitor' prev
        monitor' prev cur = timeout 1000 (waitQSemN sem 51) >>= \case
            Nothing -> tryTakeMVar output >>= monitor' prev . fromMaybe cur
            Just _ -> do
                cur'@(x, y) <- fromMaybe cur <$> tryTakeMVar output
                traceM $ printf "NAT <- (%d, %d)" x y
                modifyMVar_ channel0 $ return . (++ [cur'])
                void $ tryPutMVar unblock0 ()
                signalQSemN sem 51
                if Just y == prev then return y else monitor $ Just y
    monitor Nothing
