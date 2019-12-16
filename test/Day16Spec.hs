module Day16Spec (spec) where

import Day16 (day16a, day16b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            take 5 (day16a "12345678") `shouldBe`
                ["12345678" , "48226158" , "34040438" , "03415518" , "01029498"]
            day16a "80871224585914546619083218645595" !! 100 `shouldBe`
                "24176176"
            day16a "19617804207202209144916044189917" !! 100 `shouldBe`
                "73745418"
            day16a "69317163492948606335995924319873" !! 100 `shouldBe`
                "52432133"
    describe "part 2" $
        it "examples" $ do
            day16b "03036732577212944063491565474664" !! 100 `shouldBe`
                "84462026"
            day16b "02935109699940807407585447034323" !! 100 `shouldBe`
                "78725270"
            day16b "03081770884921959731165446850517" !! 100 `shouldBe`
                "53553731"
