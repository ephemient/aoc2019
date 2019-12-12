module Day12Spec (spec) where

import Day12 (day12a, day12b)
import Test.Hspec (Spec, describe, it, shouldBe)

sample1, sample2 :: String
sample1 = unlines
  [ "<x=-1, y=0, z=2>"
  , "<x=2, y=-10, z=-7>"
  , "<x=4, y=-8, z=8>"
  , "<x=3, y=5, z=-1>"
  ]
sample2 = unlines
  [ "<x=-8, y=-10, z=0>"
  , "<x=5, y=5, z=10>"
  , "<x=2, y=-7, z=3>"
  , "<x=9, y=-8, z=-3>"
  ]

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            (!! 10) <$> day12a sample1 `shouldBe` Right 179
            (!! 100) <$> day12a sample2 `shouldBe` Right 1940
    describe "part 2" $
        it "examples" $ do
            day12b sample1 `shouldBe` Right 2772
            day12b sample2 `shouldBe` Right 4686774924
