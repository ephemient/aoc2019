{-|
Module:         Day5
Description:    <https://adventofcode.com/2019/day/5 Day 5: Sunny with a Chance of Asteroids>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, ScopedTypeVariables, TypeApplications #-}
module Day5 (day5a, day5b, parser, run, step) where

import Control.Monad.ST (ST, runST)
import Data.Array.ST (MArray, STUArray, readArray, thaw, writeArray)
import Data.Array.Unboxed (IArray, UArray, listArray)
import Data.Ix (Ix)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Text.Megaparsec (MonadParsec, parse, sepBy)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (signed, decimal)

parser :: (IArray a e, Integral e, MonadParsec err String m) => m (a Int e)
parser = do
    ints <- signed (return ()) decimal `sepBy` char ',' <* space
    return $ listArray (0, length ints - 1) ints

step :: (Monad m, MArray a e m, Integral e, Ix i, Num i) =>
    m e -> (e -> m ()) -> a i e -> i -> m (Maybe i)
step input output mem ip = do
    op <- readArray mem ip
    let arg (n :: Int)
          | op `quot` 10 ^ (n + 1) `rem` 10 == 0
          = fromIntegral <$> readArray mem (ip + fromIntegral n)
          | otherwise = return $ ip + fromIntegral n
        get n = arg n >>= readArray mem
        put n v = arg n >>= writeArray mem `flip` v
        binOp f = do
            f <$> get 1 <*> get 2 >>= put 3
            return . Just $ ip + 4
        jmp p = p <$> get 1 >>= \case
            False -> return . Just $ ip + 3
            True -> Just . fromIntegral <$> get 2
    case op `rem` 100 of
        1 -> binOp (+)
        2 -> binOp (*)
        3 -> input >>= put 1 >> return (Just $ ip + 2)
        4 -> get 1 >>= output >> return (Just $ ip + 2)
        5 -> jmp (/= 0)
        6 -> jmp (== 0)
        7 -> binOp $ \x y -> if x < y then 1 else 0
        8 -> binOp $ \x y -> if x == y then 1 else 0
        99 -> pure Nothing
        _ -> fail "bad opcode"

run :: (Monad m, MArray a e m, Integral e, Ix i, Num i) =>
    m e -> (e -> m ()) -> a i e -> m ()
run input output mem = loop (Just 0)
  where
    loop Nothing = return ()
    loop (Just ip) = step input output mem ip >>= loop

day5a :: String -> Maybe Int
day5a input = runST $ do
    mem <- parseST input
    output <- newSTRef Nothing
    run (return 1) (writeSTRef output . Just) mem
    readSTRef output
  where
    parseST :: String -> ST s (STUArray s Int Int)
    parseST = either (fail . show) thaw . parse (parser @UArray @Int @()) ""

day5b :: String -> Maybe Int
day5b input = runST $ do
    mem <- parseST input
    output <- newSTRef Nothing
    run (return 5) (writeSTRef output . Just) mem
    readSTRef output
  where
    parseST :: String -> ST s (STUArray s Int Int)
    parseST = either (fail . show) thaw . parse (parser @UArray @Int @()) ""
