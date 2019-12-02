{-# LANGUAGE TypeApplications #-}
module Day2Spec (spec) where

import Data.Array.IO (IOUArray, freeze, getElems, newListArray, thaw)
import Data.Array.Unboxed (UArray, (//), listArray)
import Day2 (run, step)
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec =
    describe "part 1" $ do
        it "example 1" $ do
            let mem0 = listArray @UArray @Int (0, 11)
                    [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]
            mem <- thaw @Int @UArray @Int @IOUArray mem0
            step mem 0 `shouldReturn` Just 4
            freeze mem `shouldReturn` mem0 // [(3, 70)]
            step mem 4 `shouldReturn` Just 8
            freeze mem `shouldReturn` mem0 // [(3, 70), (0, 3500)]
            step mem 8 `shouldReturn` Nothing
            freeze mem `shouldReturn` mem0 // [(3, 70), (0, 3500)]
        it "example 2" $ do
            mem <- newListArray @IOUArray @Int @IO @Int (0, 4) [1, 0, 0, 0, 99]
            run mem
            getElems mem `shouldReturn` [2, 0, 0, 0, 99]
        it "example 3" $ do
            mem <- newListArray @IOUArray @Int @IO @Int (0, 4) [2, 3, 0, 3, 99]
            run mem
            getElems mem `shouldReturn` [2, 3, 0, 6, 99]
        it "example 4" $ do
            mem <- newListArray @IOUArray @Int @IO @Int (0, 5)
                [2, 4, 4, 5, 99, 0]
            run mem
            getElems mem `shouldReturn` [2, 4, 4, 5, 99, 9801]
        it "example 5" $ do
            mem <- newListArray @IOUArray @Int @IO @Int (0, 8)
                [1, 1, 1, 4, 99, 5, 6, 0, 99]
            run mem
            getElems mem `shouldReturn` [30, 1, 1, 4, 2, 5, 6, 0, 99]
