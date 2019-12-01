module Day1Spec (spec) where

import Day1 (day1a, day1b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day1a "12" `shouldBe` 2
            day1a "14" `shouldBe` 2
            day1a "1969" `shouldBe` 654
            day1a "100756" `shouldBe` 33583
    describe "part 2" $
        it "examples" $ do
            day1b "14" `shouldBe` 2
            day1b "1969" `shouldBe` 966
            day1b "100756" `shouldBe` 50346
