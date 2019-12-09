{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Reduce duplication" #-}
module IntcodeSpec (spec) where

import Data.Array.IO (IOUArray, getElems, newListArray, readArray, writeArray)
import Intcode (Memory(..))
import qualified Intcode (run)
import Test.Hspec (Spec, describe, it, shouldReturn)
import Test.QuickCheck ((===), arbitrary)
import Test.QuickCheck.Monadic (monadicIO, pick, run)

runMem :: [Int] -> IO [Int]
runMem ints = do
    mem <- newListArray @IOUArray @Int @IO @Int (0, length ints - 1) ints
    let memory = Memory { readMem = readArray mem , writeMem = writeArray mem }
    Intcode.run memory [] `shouldReturn` []
    getElems mem

compile :: [Int] -> [Int] -> IO [Int]
compile code input = do
    mem <- newListArray @IOUArray (0, length code - 1) code
    let memory = Memory { readMem = readArray mem , writeMem = writeArray mem }
    Intcode.run memory input

spec :: Spec
spec = do
    describe "day 2 part 1" $ do
        it "example 1" $
            runMem [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] `shouldReturn`
                [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
        it "example 2" $ runMem [1, 0, 0, 0, 99] `shouldReturn` [2, 0, 0, 0, 99]
        it "example 3" $
            runMem [2, 3, 0, 3, 99] `shouldReturn` [2, 3, 0, 6, 99]
        it "example 4" $
            runMem [2, 4, 4, 5, 99, 0] `shouldReturn` [2, 4, 4, 5, 99, 9801]
        it "example 5" $ runMem [1, 1, 1, 4, 99, 5, 6, 0, 99] `shouldReturn`
            [30, 1, 1, 4, 2, 5, 6, 0, 99]
    describe "day 5 part 1" $ do
        it "example 1" $
            runMem [1002, 4, 3, 4, 33] `shouldReturn` [1002, 4, 3, 4, 99]
        it "example 2" $
            runMem [1101, 100, -1, 4, 0] `shouldReturn` [1101, 100, -1, 4, 99]
    describe "day 5 part 2" $ do
        it "example 1" $
            let intcode = compile [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]
            in monadicIO $ do
                x <- pick arbitrary
                output <- run $ intcode [x]
                return $ output === [fromEnum $ x == 8]
        it "example 2" $
            let intcode = compile [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8]
            in monadicIO $ do
                x <- pick arbitrary
                output <- run $ intcode [x]
                return $ output === [fromEnum $ x < 8]
        it "example 3" $
            let intcode = compile [3, 3, 1108, -1, 8, 3, 4, 3, 99]
            in monadicIO $ do
                x <- pick arbitrary
                output <- run $ intcode [x]
                return $ output === [fromEnum $ x == 8]
        it "example 4" $
            let intcode = compile [3, 3, 1107, -1, 8, 3, 4, 3, 99]
            in monadicIO $ do
                x <- pick arbitrary
                output <- run $ intcode [x]
                return $ output === [fromEnum $ x < 8]
        it "example 5" $
            let intcode = compile
                    [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9]
            in monadicIO $ do
                x <- pick arbitrary
                output <- run $ intcode [x]
                return $ output === [fromEnum $ x /= 0]
        it "example 6" $
            let intcode = compile
                    [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1]
            in monadicIO $ do
                x <- pick arbitrary
                output <- run $ intcode [x]
                return $ output === [fromEnum $ x /= 0]
        it "example 7" $
            let intcode = compile
                  [ 3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006
                  , 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20
                  , 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4
                  , 20, 1105, 1, 46, 98, 99
                  ]
            in monadicIO $ do
                x <- pick arbitrary
                output <- run $ intcode [x]
                return $ output === [999 + fromEnum (compare x 8)]
    describe "day 9 part 1" $
        it "examples" $ do
            let quine =
                  [ 109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006
                  , 101, 0, 99 ]
            compile (quine ++ replicate 86 0) [] `shouldReturn` quine
            compile [1102, 34915192, 34915192, 7, 4, 7, 99, 0] []
                `shouldReturn` [1219070632396864]
            compile [104, 1125899906842624, 99] [] `shouldReturn`
                [1125899906842624]
