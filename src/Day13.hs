{-|
Module:         Day13
Description:    <https://adventofcode.com/2019/day/13 Day 13: Care Package>
-}
{-# LANGUAGE FlexibleContexts, GADTs, LambdaCase, NamedFieldPuns, NoMonomorphismRestriction, ParallelListComp, PatternGuards, RecordWildCards, ScopedTypeVariables, TransformListComp, TupleSections, TypeApplications, TypeFamilies, ViewPatterns #-}
module Day13 (day13a, day13b) where

import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.ST (runST)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, empty, insert, lookup)
import Data.Primitive.MutVar (modifyMutVar, newMutVar, readMutVar, writeMutVar)
import Data.Vector.Generic (Vector, (//), fromList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Intcode (Context(..), step)
import Intcode.Vector (memory)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (Vector v e, Integral e, MonadParsec err String m) => m (v e)
parser = fromList <$> (signed (return ()) decimal `sepBy` char ',' <* space)

play :: (Integral e, Ord e, Vector v e, PrimMonad m) => v e -> m (Map (e, e) e)
play mem0 = do
    display <- newMutVar Map.empty
    paddleX <- newMutVar Nothing
    ballX <- newMutVar Nothing
    mem <- memory mem0
    let context0 = Context
          { next = step mem context0
          , output = step mem . context1
          , terminate
          }
        context1 x = fix $ \context -> Context
          { next = step mem context
          , output = step mem . context2 x
          , terminate
          }
        context2 x y = fix $ \context -> Context
          { next = step mem context
          , output = \e _ base ip -> do
                modifyMutVar display $ Map.insert (x, y) e
                paddle <- if e == 3
                    then writeMutVar paddleX (Just x) $> (Just x)
                    else readMutVar paddleX
                ball <- if e == 4
                    then writeMutVar ballX (Just x) $> (Just x)
                    else readMutVar ballX
                let input = case compare <$> paddle <*> ball of
                        Just LT -> repeat 1
                        Just GT -> repeat (-1)
                        _ -> repeat 0
                step mem context0 input base ip
          , terminate
          }
        terminate _ _ _ = readMutVar display
    step mem context0 (repeat 0) 0 0

day13a :: String -> Either (ParseErrorBundle String ()) Int
day13a input = do
    mem0 <- parse (parser @Unboxed.Vector @Int) "" input
    return $ length $ filter (== 2) $ Map.elems $ runST (play mem0)

day13b :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day13b input = do
    mem0 <- parse (parser @Unboxed.Vector @Int) "" input
    return $ Map.lookup (-1, 0) $ runST (play $ mem0 // [(0, 2)])
