module Day4Spec (spec) where

import Day4 (day4a, day4b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day4a "111111-111111" `shouldBe` Right 1
            day4a "223450-223450" `shouldBe` Right 0
            day4a "123789-123789" `shouldBe` Right 0
    describe "part 2" $
        it "examples" $ do
            day4b "112233-112233" `shouldBe` Right 1
            day4b "123444-123444" `shouldBe` Right 0
            day4b "111122-111122" `shouldBe` Right 1
