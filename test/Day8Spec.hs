module Day8Spec (spec) where

import Day8 (day8b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "part 2" $
        it "examples" $
            day8b 2 2 "0222112222120000" `shouldBe` "\x2592\x2593\n\x2593\x2592"
