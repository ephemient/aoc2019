{-# LANGUAGE TypeApplications #-}
module Day23Spec (spec) where

import Data.List (intercalate)
import Day23 (day23a, day23b)
import Test.Hspec (Spec, describe, it, shouldBe)

sample :: String
sample = intercalate "," $ show @Int <$>
  [ 3, 0                --  0: in [0]
  , 1006, 0, 51         --  2: jz [0], 51
  , 3, 2                --  5: in [2]
  , 1007, 2, 0, 3       --  7: [3] = [2] < 0
  , 1005, 3, 5          -- 11: jnz [3], 5
  , 3, 3                -- 14: in [3]
  , 1006, 0, 58         -- 16: jz [0], 58
  , 101, 1, 0, 1        -- 19: [1] = 1 + [0]
  , 101, 1, 2, 2        -- 23: [2] = 1 + [2]
  , 1, 2, 3, 3          -- 27: [3] = [2] + [3]
  , 1007, 1, 50, 4      -- 31: [4] = [1] < 50
  , 1005, 4, 42         -- 35: jnz [4], 42
  , 1102, 5, 51, 1      -- 38: [1] = 5 * 51
  , 4, 1                -- 42: out [1]
  , 4, 2                -- 44: out [2]
  , 4, 3                -- 46: out [3]
  , 1106, 0, 5          -- 48: jz 0, 5
  , 1101, 0, 1, 1       -- 51: [1] = 0 + 1
  , 1106, 0, 42         -- 55: jz 0, 42
  , 101, 0, 2, 3        -- 58: [3] = 0 + [2]
  , 1101, 0, 0, 2       -- 62: [2] = 0 + 0
  , 1106, 0, 42         -- 66: jz 0, 42
  ]

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day23a sample `shouldBe` Right (Just 50519)
    describe "part 2" $
        it "examples" $
            day23b sample `shouldBe` Right (Just 1274)
