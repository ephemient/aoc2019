{-|
Module:         Day13
Description:    <https://adventofcode.com/2019/day/13 Day 13: Care Package>
-}
{-# LANGUAGE BlockArguments, FlexibleContexts, NondecreasingIndentation, TypeApplications #-}
module Day13 (day13a, day13b) where

import Control.Monad.ST (runST)
import qualified Data.Set as Set (delete, empty, insert, size)
import Data.Vector.Generic (Vector, (//), fromList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Intcode (evalIntcodeT, getOutput, setInput)
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
    let loop acc' paddle ball = getOutput >>= maybe (return acc') \x ->
            getOutput >>= maybe (return acc') \y ->
            getOutput >>= maybe (return acc') \e -> do
            let acc'' = f x y e acc'
                paddle' = if e == 3 then Just x else paddle
                ball' = if e == 4 then Just x else ball
            setInput $ return $ case compare <$> paddle' <*> ball' of
                Just LT -> 1
                Just GT -> -1
                _ -> 0
            loop acc'' paddle' ball'
    evalIntcodeT (loop acc Nothing Nothing) mem $ return 0

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
