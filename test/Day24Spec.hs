module Day24Spec (spec) where

import Day24 (day24a, day24b)
import Test.Hspec (Spec, describe, it, shouldBe)

sample :: String
sample = unlines ["....#", "#..#.", "#..##", "..#..", "#...."]

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day24a sample `shouldBe` Just 2129920
    describe "part 2" $
        it "examples" $
            day24b sample !! 10 `shouldBe` 99
