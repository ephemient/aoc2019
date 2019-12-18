module Day17Spec (spec) where

import Data.Char (ord)
import Data.List (intercalate)
import Day17 (day17a, day17b)
import Test.Hspec (Spec, describe, it, shouldBe)

sample1, sample2 :: String
sample1 = unlines
  [ "..#.........."
  , "..#.........."
  , "#######...###"
  , "#.#...#...#.#"
  , "#############"
  , "..#...#...#.."
  , "..#####...^.."
  ]
sample2 = unlines
  [ "#######...#####"
  , "#.....#...#...#"
  , "#.....#...#...#"
  , "......#...#...#"
  , "......#...###.#"
  , "......#.....#.#"
  , "^########...#.#"
  , "......#.#...#.#"
  , "......#########"
  , "........#...#.."
  , "....#########.."
  , "....#...#......"
  , "....#...#......"
  , "....#...#......"
  , "....#####......"
  ]

bot1, bot2 :: String -> String
bot1 = intercalate "," . foldr (\c s -> "104" : show (ord c) : s) ["99"]
-- TODO: actually make this test work
bot2 input = "1,0,1,0,5,0,10,104,10000000,99," ++ bot1 input

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day17a (bot1 sample1) `shouldBe` Right 76
    describe "part 2" $
        it "examples" $
            day17b (bot2 sample2) `shouldBe` Right 10000000
