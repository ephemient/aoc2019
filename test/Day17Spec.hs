{-# LANGUAGE TypeApplications #-}
module Day17Spec (spec) where

import Data.Char (ord)
import Data.List (intercalate)
import qualified Data.Vector.Generic as Vector (fromList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Day17 (day17a, draw, paths, programs)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

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

bot :: String -> [Int]
bot = foldr (\c s -> 104 : ord c : s) [99]

showInts :: [Int] -> String
showInts = intercalate "," . map show

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day17a (showInts $ bot sample1) `shouldBe` Right 76
    describe "part 2" $
        it "examples" $ do
            let mem0 = Vector.fromList @Unboxed.Vector $ bot sample2
                (points, start, direction) = draw mem0
                allPaths = paths points start direction
                (ids, programs'):_ = allPaths >>=
                    programs 10 ((<= 20) . length . show) [0..2]
            ids `shouldSatisfy` ((<= 10) . length)
            programs' `shouldSatisfy` ((== 3) . length)
            mapM_ (`shouldSatisfy` ((<= 20) . length . show)) programs'
            intercalate "," (map (show . (programs' !!)) ids) `shouldBe`
                "R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2"
