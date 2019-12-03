module Day3Spec (spec) where

import Day3 (day3a, day3b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day3a sample1 `shouldBe` Right 6
            day3a sample2 `shouldBe` Right 159
            day3a sample3 `shouldBe` Right 135
    describe "part 2" $
        it "examples" $ do
            day3b sample1 `shouldBe` Right 30
            day3b sample2 `shouldBe` Right 610
            day3b sample3 `shouldBe` Right 410
  where
    sample1 = unlines ["R8,U5,L5,D3", "U7,R6,D4,L4"]
    sample2 = unlines
      [ "R75,D30,R83,U83,L12,D49,R71,U7,L72"
      , "U62,R66,U55,R34,D71,R55,D58,R83"
      ]
    sample3 = unlines
      [ "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
      , "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
      ]
