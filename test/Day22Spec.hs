{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module Day22Spec (spec) where

import Common (egcd)
import Data.List (foldl')
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as Vector
import qualified Data.Vector.Unboxed as Unboxed
import Data.Void (Void)
import Day22 (Operation(..), applyTimes, mpow, parser)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parseMaybe)

doOp :: (Vector v a, Vector v Int) => v a -> Operation Int -> v a
doOp s Reverse = Vector.reverse s
doOp s (Cut n) = uncurry (flip (Vector.++)) $ Vector.splitAt n' s where
    n' = n `mod` Vector.length s
doOp s (Stretch n) = Vector.backpermute s $ Vector.iterateN p inc 0 where
    p = Vector.length s
    (n', _, 1) = egcd n p
    inc x = (x + n') `mod` p

referenceApply, testApply :: (Foldable t) =>
    Int -> t (Operation Int) -> Int -> Unboxed.Vector Int
referenceApply p ops x =
    iterate (foldl' doOp `flip` ops) (Vector.enumFromN 0 p) !! x
testApply p ops x = Vector.generate p $ \i -> (m * i + c) `mod` p where
    (m, c) = mpow p (applyTimes p ops x) (-1 :: Int)

sample1, sample2, sample3, sample4 :: String
sample1 = unlines
    ["deal with increment 7" , "deal into new stack" , "deal into new stack"]
sample2 = unlines ["cut 6" , "deal with increment 7" , "deal into new stack"]
sample3 = unlines ["deal with increment 7" , "deal with increment 9" , "cut -2"]
sample4 = unlines
  [ "deal into new stack"
  , "cut -2"
  , "deal with increment 7"
  , "cut 8"
  , "cut -4"
  , "deal with increment 7"
  , "cut 3"
  , "deal with increment 9"
  , "deal with increment 3"
  , "cut -1"
  ]

spec :: Spec
spec = do
    let Just ops1 = parseMaybe @Void parser sample1
        Just ops2 = parseMaybe @Void parser sample2
        Just ops3 = parseMaybe @Void parser sample3
        Just ops4 = parseMaybe @Void parser sample4
    describe "part 1" $ do
        it "examples (reference implementation)" $ do
            referenceApply 10 ops1 1 `shouldBe`
                Vector.fromList [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]
            referenceApply 10 ops2 1 `shouldBe`
                Vector.fromList [3, 0, 7, 4, 1, 8, 5, 2, 9, 6]
            referenceApply 10 ops3 1 `shouldBe`
                Vector.fromList [6, 3, 0, 7, 4, 1, 8, 5, 2, 9]
            referenceApply 10 ops4 1 `shouldBe`
                Vector.fromList [9, 2, 5, 8, 1, 4, 7, 0, 3, 6]
        it "examples (implementation under test)" $ do
            testApply 10 ops1 1 `shouldBe`
                Vector.fromList [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]
            testApply 10 ops2 1 `shouldBe`
                Vector.fromList [3, 0, 7, 4, 1, 8, 5, 2, 9, 6]
            testApply 10 ops3 1 `shouldBe`
                Vector.fromList [6, 3, 0, 7, 4, 1, 8, 5, 2, 9]
            testApply 10 ops4 1 `shouldBe`
                Vector.fromList [9, 2, 5, 8, 1, 4, 7, 0, 3, 6]
