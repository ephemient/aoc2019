{-|
Module:         Day13
Description:    <https://adventofcode.com/2019/day/13 Day 13: Care Package>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module Day13 (day13a, day13b) where

import Control.Monad.Fix (fix)
import Control.Monad.ST (runST)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, empty, insert, lookup)
import Data.Vector.Generic (Vector, (//), fromList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Intcode (Context(..), step)
import Intcode.Vector (memory)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (Vector v e, Integral e, MonadParsec err String m) => m (v e)
parser = fromList <$> (signed (return ()) decimal `sepBy` char ',' <* space)

play :: (Integral e, Ord e, Vector v e, Show e) => v e -> Map (e, e) e
play mem0 = runST $ do
    mem <- memory mem0
    let context0 display paddle ball = fix $ \context -> Context
          { next = step mem context
          , output = step mem . context1 display paddle ball
          , terminate = \_ _ _ -> return display
          }
        context1 display paddle ball x = fix $ \context -> Context
          { next = step mem context
          , output = step mem . context2 display paddle ball x
          , terminate = \_ _ _ -> return display
          }
        context2 display paddle ball x y = fix $ \context -> Context
          { next = step mem context
          , output = \e _ base ip -> do
                let display' = Map.insert (x, y) e display
                    paddle' = if e == 3 then Just x else paddle
                    ball' = if e == 4 then Just x else ball
                    input = case compare <$> paddle' <*> ball' of
                        Just LT -> repeat 1
                        Just GT -> repeat (-1)
                        _ -> repeat 0
                step mem (context0 display' paddle' ball') input base ip
          , terminate = \_ _ _ -> return display
          }
    step mem (context0 Map.empty Nothing Nothing) (repeat 0) 0 0

day13a :: String -> Either (ParseErrorBundle String ()) Int
day13a input = do
    mem0 <- parse (parser @Unboxed.Vector @Int) "" input
    return $ length $ filter (== 2) $ Map.elems $ play mem0

day13b :: String -> Either (ParseErrorBundle String ()) (Maybe Int)
day13b input = do
    mem0 <- parse (parser @Unboxed.Vector @Int) "" input
    return $ Map.lookup (-1, 0) $ play $ mem0 // [(0, 2)]
