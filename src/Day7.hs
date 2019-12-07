{-|
Module:         Day7
Description:    <https://adventofcode.com/2019/day/7 Day 7: Amplification Circuit>
-}
{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, ScopedTypeVariables, TypeApplications, ViewPatterns #-}
module Day7 (amplify, day7a, day7b) where

import Control.Concurrent (forkIO, newChan, newEmptyMVar, putMVar, readChan, takeMVar, writeChan)
import Control.Monad (foldM)
import Data.Array.IO (IOUArray, MArray, thaw)
import Data.Array.Unboxed (IArray, UArray)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Ix (Ix)
import Data.List (permutations)
import Day5 (parser, run)
import Text.Megaparsec (parseMaybe)

amplify :: forall a b e i. (IArray a e, MArray b e IO, Integral e, Ix i, Num i) =>
    a i e -> [e] -> IO (Maybe e)
amplify mem0 order = do
    chans <- mapM (const newChan) order
    let start n input output = do
            mem <- thaw @i @a @e @b mem0
            done <- newEmptyMVar
            _ <- forkIO $ do
                lastOutput <- newIORef Nothing
                let writeAndLog x = do
                        writeIORef lastOutput $ Just x
                        writeChan output x
                run (readChan input) writeAndLog mem
                readIORef lastOutput >>= putMVar done
            writeChan input n
            return done
    dones <- sequence $ zipWith3 start order chans $ drop 1 $ cycle chans
    writeChan (head chans) 0
    takeMVar $ last dones

day7a :: String -> IO (Maybe Int)
day7a (parseMaybe @() parser -> Just mem) =
    foldM (\a -> fmap (max a) . amplify @UArray @IOUArray mem) Nothing $
    permutations [0..4]
day7a _ = return Nothing

day7b :: String -> IO (Maybe Int)
day7b (parseMaybe @() parser -> Just mem) =
    foldM (\a -> fmap (max a) . amplify @UArray @IOUArray mem) Nothing $
    permutations [5..9]
day7b _ = return Nothing
