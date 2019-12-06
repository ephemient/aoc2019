module Day6Spec (spec) where

import Day6 (day6a, day6b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day6a (unlines
              [ "COM)B"
              , "B)C"
              , "C)D"
              , "D)E"
              , "E)F"
              , "B)G"
              , "G)H"
              , "D)I"
              , "E)J"
              , "J)K"
              , "K)L"
              ]
            ) `shouldBe` Just 42
    describe "part 2" $
        it "examples" $
            day6b (unlines
              [ "COM)B"
              , "B)C"
              , "C)D"
              , "D)E"
              , "E)F"
              , "B)G"
              , "G)H"
              , "D)I"
              , "E)J"
              , "J)K"
              , "K)L"
              , "K)YOU"
              , "I)SAN"
              ]
            ) `shouldBe` Just 4
