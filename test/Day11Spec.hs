{-# LANGUAGE ViewPatterns #-}
module Day11Spec (spec) where

import Day11 (day11a)
import Test.Hspec (Spec, describe, it, shouldBe)

exampleBot :: String
exampleBot = "3,0,5,0,-1,104,1,104,0,3,0,5,0,-1,104,0,104,0,3,0,104,1,104,0," ++
    "3,0,104,1,104,0,3,0,6,0,-1,104,0,104,1,3,0,104,1,104,0,3,0,104,1,104,0,99"

spec :: Spec
spec =
    describe "part 1" $
        it "examples" $
            day11a exampleBot `shouldBe` Right 6
