{-|
Module:         Day22
Description:    <https://adventofcode.com/2019/day/22 Day 22: Slam Shuffle>
-}
{-# LANGUAGE FlexibleContexts #-}
module Day22 (Operation(..), applyTimes, day22a, day22b, egcd, mpow, parser) where

import Data.Functor (($>), (<&>))
import Data.List (foldl')
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, choice, eof, parse, sepEndBy)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Operation a = Reverse | Cut a | Stretch a deriving (Eq, Show)
instance Functor Operation where
    fmap _ Reverse = Reverse
    fmap f (Cut n) = Cut $ f n
    fmap f (Stretch n) = Stretch $ f n

parser :: (Integral a, MonadParsec err String m) => m [Operation a]
parser = choice
  [ string "deal into new stack" $> Reverse
  , string "cut " *> signed (return ()) decimal <&> Cut
  , string "deal with increment " *> decimal <&> Stretch
  ] `sepEndBy` newline <* eof

-- |Extended GCD.
--
-- prop> gcd a b == (s, t, g) ==> a * s + b * t == g
egcd :: (Integral a) => a -> a -> (a, a, a)
egcd a 0 = (1, 0, a)
egcd a b = (t, s - q * t, abs g) where
    (q, r) = a `quotRem` b
    (s, t, g) = egcd b r

apply :: (Integral a) => a -> Operation a -> (a, a) -> (a, a)
apply p Reverse (m, c) = ((-m) `mod` p, (-c - 1) `mod` p)
apply p (Cut n) (m, c) = (m, (c - n) `mod` p)
apply p (Stretch n) (m, c) = (m * n `mod` p, c * n `mod` p)

applyTimes :: (Integral a, Integral b, Foldable t) =>
    a -> t (Operation a) -> b -> (a, a)
applyTimes p = mpow p . foldl' (flip $ apply p) (1, 0)

mmul :: (Integral a) => a -> (a, a) -> (a, a) -> (a, a)
mmul p (a, b) (c, d) = (a * c `mod` p, (a * d + b) `mod` p)

mpow :: (Integral a, Integral b) => a -> (a, a) -> b -> (a, a)
mpow p (m, c) x | x < 0 = mpow p (n, - n * c `mod` p) (-x) where
    (n, _, 1) = egcd m p
mpow _ _ 0 = (1, 0)
mpow _ z 1 = z
mpow p z x = f $ mpow p (mmul p z z) q where
    (q, r) = x `divMod` 2
    f = if r == 0 then id else mmul p z

day22a :: String -> Either (ParseErrorBundle String Void) Integer
day22a input = do
    ops <- parse parser "" input
    let p = 10007
        (m, c) = applyTimes p ops (1 :: Int)
    return $ (m * 2019 + c) `mod` p

day22b :: String -> Either (ParseErrorBundle String Void) Integer
day22b input = do
    ops <- parse parser "" input
    let p = 119315717514047
        (m, c) = applyTimes p ops (-101741582076661 :: Int)
    return $ (m * 2020 + c) `mod` p
