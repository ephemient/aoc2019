module Day14Spec (spec) where

import Day14 (day14a, day14b)
import Test.Hspec (Spec, describe, it, pendingWith, shouldReturn)

sample1, sample2, sample3, sample4, sample5 :: String
sample1 = unlines
  [ "10 ORE => 10 A"
  , "1 ORE => 1 B"
  , "7 A, 1 B => 1 C"
  , "7 A, 1 C => 1 D"
  , "7 A, 1 D => 1 E"
  , "7 A, 1 E => 1 FUEL"
  ]
sample2 = unlines
  [ "9 ORE => 2 A"
  , "8 ORE => 3 B"
  , "7 ORE => 5 C"
  , "3 A, 4 B => 1 AB"
  , "5 B, 7 C => 1 BC"
  , "4 C, 1 A => 1 CA"
  , "2 AB, 3 BC, 4 CA => 1 FUEL"
  ]
sample3 = unlines
  [ "157 ORE => 5 NZVS"
  , "165 ORE => 6 DCFZ"
  , "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
  , "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
  , "179 ORE => 7 PSHF"
  , "177 ORE => 5 HKGWZ"
  , "7 DCFZ, 7 PSHF => 2 XJWVT"
  , "165 ORE => 2 GPVTF"
  , "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
  ]
sample4 = unlines
  [ "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG"
  , "17 NVRVD, 3 JNWZP => 8 VPVL"
  , "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL"
  , "22 VJHF, 37 MNCFX => 5 FWMGM"
  , "139 ORE => 4 NVRVD"
  , "144 ORE => 7 JNWZP"
  , "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC"
  , "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV"
  , "145 ORE => 6 MNCFX"
  , "1 NVRVD => 8 CXFTF"
  , "1 VJHF, 6 MNCFX => 4 RFSQX"
  , "176 ORE => 6 VJHF"
  ]
sample5 = unlines
  [ "171 ORE => 8 CNZTR"
  , "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL"
  , "114 ORE => 4 BHXH"
  , "14 VRPVC => 6 BMBT"
  , "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL"
  , "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT"
  , "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW"
  , "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW"
  , "5 BMBT => 4 WPTQ"
  , "189 ORE => 9 KTJDG"
  , "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP"
  , "12 VRPVC, 27 CNZTR => 2 XDBXC"
  , "15 KTJDG, 12 BHXH => 5 XCVML"
  , "3 BHXH, 2 VRPVC => 7 MZWV"
  , "121 ORE => 7 VRPVC"
  , "7 XCVML => 6 RJRHP"
  , "5 BHXH, 4 VRPVC => 5 LTCX"
  ]

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day14a sample1 `shouldReturn` 31
            day14a sample2 `shouldReturn` 165
            day14a sample3 `shouldReturn` 13312
            day14a sample4 `shouldReturn` 180697
            day14a sample5 `shouldReturn` 2210736
    describe "part 2" $ do
        it "failing examples" $ do
            pendingWith "GLPK comes up short"
            day14b sample3 `shouldReturn` 82892753
        it "examples" $ do
            day14b sample4 `shouldReturn` 5586022
            day14b sample5 `shouldReturn` 460664
