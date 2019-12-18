module Day18Spec (spec) where

import Day18 (day18a, day18b)
import Test.Hspec (Spec, describe, it, shouldBe)

sample1, sample2, sample3, sample4, sample5, sample6, sample7, sample8, sample9 :: String
sample1 = unlines
  [ "#########"
  , "#b.A.@.a#"
  , "#########"
  ]
sample2 = unlines
  [ "########################"
  , "#f.D.E.e.C.b.A.@.a.B.c.#"
  , "######################.#"
  , "#d.....................#"
  , "########################"
  ]
sample3 = unlines
  [ "########################"
  , "#...............b.C.D.f#"
  , "#.######################"
  , "#.....@.a.B.c.d.A.e.F.g#"
  , "########################"
  ]
sample4 = unlines
  [ "#################"
  , "#i.G..c...e..H.p#"
  , "########.########"
  , "#j.A..b...f..D.o#"
  , "########@########"
  , "#k.E..a...g..B.n#"
  , "########.########"
  , "#l.F..d...h..C.m#"
  , "#################"
  ]
sample5 = unlines
  [ "########################"
  , "#@..............ac.GI.b#"
  , "###d#e#f################"
  , "###A#B#C################"
  , "###g#h#i################"
  , "########################"
  ]
sample6 = unlines
  [ "#######"
  , "#a.#Cd#"
  , "##...##"
  , "##.@.##"
  , "##...##"
  , "#cB#Ab#"
  , "#######"
  ]
sample7 = unlines
  [ "###############"
  , "#d.ABC.#.....a#"
  , "######...######"
  , "######.@.######"
  , "######...######"
  , "#b.....#.....c#"
  , "###############"
  ]
sample8 = unlines
  [ "#############"
  , "#DcBa.#.GhKl#"
  , "#.###...#I###"
  , "#e#d#.@.#j#k#"
  , "###C#...###J#"
  , "#fEbA.#.FgHi#"
  , "#############"
  ]
sample9 = unlines
  [ "#############"
  , "#g#f.D#..h#l#"
  , "#F###e#E###.#"
  , "#dCba...BcIJ#"
  , "#####.@.#####"
  , "#nK.L...G...#"
  , "#M###N#H###.#"
  , "#o#m..#i#jk.#"
  , "#############"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "example 1" $ day18a sample1 `shouldBe` Just 8
        it "example 2" $ day18a sample2 `shouldBe` Just 86
        it "example 3" $ day18a sample3 `shouldBe` Just 132
        it "example 4" $ day18a sample4 `shouldBe` Just 136
        it "example 5" $ day18a sample5 `shouldBe` Just 81
    describe "part 2" $ do
        it "example 1" $ day18b sample6 `shouldBe` Just 8
        it "example 2" $ day18b sample7 `shouldBe` Just 24
        it "example 3" $ day18b sample8 `shouldBe` Just 32
        it "example 4" $ day18b sample9 `shouldBe` Just 72
