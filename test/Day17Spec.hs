module Day17Spec (spec) where

import Data.Char (ord)
import Data.List (intercalate)
import Day17 (day17a)
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

spec :: Spec
spec =
    describe "part 1" $
        it "examples" $ do
            let bot = intercalate "," $
                    foldr (\c -> ("104" :) . (show (ord c) :)) ["99"] sample1
            day17a bot `shouldBe` Right 76
