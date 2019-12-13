{-|
Module:         Day13
Description:    <https://adventofcode.com/2019/day/13 Day 13: Care Package>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module Day13 (day13a, day13b) where

import Control.Monad.Fix (fix)
import Control.Monad.ST (runST)
import qualified Data.Set as Set (delete, empty, insert, size)
import Data.Vector.Generic (Vector, (//), fromList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Intcode (Context(..), step)
import Intcode.Vector (memory)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (Vector v e, Integral e, MonadParsec err String m) => m (v e)
parser = fromList <$> (signed (return ()) decimal `sepBy` char ',' <* space)

play :: (Integral e, Ord e, Vector v e, Show e) =>
    (e -> e -> e -> a -> a) -> a -> v e -> a
play f acc mem0 = runST $ do
    mem <- memory mem0
    let context0 acc' paddle ball = fix $ \context -> Context
          { next = step mem context
          , output = step mem . context1 acc' paddle ball
          , terminate = \_ _ _ -> return acc'
          }
        context1 acc' paddle ball x = fix $ \context -> Context
          { next = step mem context
          , output = step mem . context2 acc' paddle ball x
          , terminate = \_ _ _ -> return acc'
          }
        context2 acc' paddle ball x y = fix $ \context -> Context
          { next = step mem context
          , output = \e _ base ip -> do
                let acc'' = f x y e acc'
                    paddle' = if e == 3 then Just x else paddle
                    ball' = if e == 4 then Just x else ball
                    input = case compare <$> paddle' <*> ball' of
                        Just LT -> repeat 1
                        Just GT -> repeat (-1)
                        _ -> repeat 0
                step mem (context0 acc'' paddle' ball') input base ip
          , terminate = \_ _ _ -> return acc'
          }
    step mem (context0 acc Nothing Nothing) (repeat 0) 0 0

day13a :: String -> Either (ParseErrorBundle String ()) Int
day13a input = do
    mem0 <- parse (parser @Unboxed.Vector @Int) "" input
    let f x y 2 = Set.insert (x, y)
        f x y _ = Set.delete (x, y)
    return $ Set.size $ play f Set.empty mem0

day13b :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day13b input = do
    mem0 <- parse (parser @Unboxed.Vector @Int) "" input
    let f (-1) 0 = const . Just
        f _ _ = const id
    return $ play f Nothing $ mem0 // [(0, 2)]
