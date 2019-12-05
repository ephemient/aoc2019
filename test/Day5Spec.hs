{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Reduce duplication" #-}
module Day5Spec (spec) where

import Data.Array.IO (IOUArray, freeze, newListArray, thaw)
import Data.Array.Unboxed (UArray, (//), listArray)
import Data.IORef (newIORef, readIORef, writeIORef)
import Day5 (step)
import qualified Day5 (run)
import Test.Hspec (Spec, describe, it, shouldReturn)
import Test.QuickCheck ((===), arbitrary)
import Test.QuickCheck.Monadic (monadicIO, pick, run)

compile :: [Int] -> Int -> IO (Maybe Int)
compile code arg = do
    mem <- newListArray @IOUArray (0, length code - 1) code
    output <- newIORef Nothing
    Day5.run (return arg) (writeIORef output . Just) mem
    readIORef output

spec :: Spec
spec = do
    describe "part 1" $ do
        it "example 1" $ do
            let mem0 = listArray @UArray @Int (0, 4) [1002, 4, 3, 4, 33]
                input = fail "no input"
                output e = fail $ "no output " ++ show e
            mem <- thaw @Int @UArray @Int @IOUArray mem0
            step input output mem 0 `shouldReturn` Just 4
            freeze mem `shouldReturn` mem0 // [(4, 99)]
            step input output mem 4 `shouldReturn` Nothing
            freeze mem `shouldReturn` mem0 // [(4, 99)]
        it "example 2" $ do
            let mem0 = listArray @UArray @Int (0, 4) [1101, 100, -1, 4, 0]
                input = fail "no input"
                output e = fail $ "no output " ++ show e
            mem <- thaw @Int @UArray @Int @IOUArray mem0
            step input output mem 0 `shouldReturn` Just 4
            freeze mem `shouldReturn` mem0 // [(4, 99)]
            step input output mem 4 `shouldReturn` Nothing
            freeze mem `shouldReturn` mem0 // [(4, 99)]
    describe "part 2" $ do
        it "example 1" $
            let intcode = compile [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]
            in monadicIO $ do
                x <- pick arbitrary
                output <- run $ intcode x
                return $ output === Just (fromEnum $ x == 8)
        it "example 2" $
            let intcode = compile [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8]
            in monadicIO $ do
                x <- pick arbitrary
                output <- run $ intcode x
                return $ output === Just (fromEnum $ x < 8)
        it "example 3" $
            let intcode = compile [3, 3, 1108, -1, 8, 3, 4, 3, 99]
            in monadicIO $ do
                x <- pick arbitrary
                output <- run $ intcode x
                return $ output === Just (fromEnum $ x == 8)
        it "example 4" $
            let intcode = compile [3, 3, 1107, -1, 8, 3, 4, 3, 99]
            in monadicIO $ do
                x <- pick arbitrary
                output <- run $ intcode x
                return $ output === Just (fromEnum $ x < 8)
        it "example 5" $
            let intcode = compile
                    [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9]
            in monadicIO $ do
                x <- pick arbitrary
                output <- run $ intcode x
                return $ output === Just (fromEnum $ x /= 0)
        it "example 6" $
            let intcode = compile
                    [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1]
            in monadicIO $ do
                x <- pick arbitrary
                output <- run $ intcode x
                return $ output === Just (fromEnum $ x /= 0)
        it "example 7" $
            let intcode = compile
                  [ 3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006
                  , 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20
                  , 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4
                  , 20, 1105, 1, 46, 98, 99
                  ]
            in monadicIO $ do
                x <- pick arbitrary
                output <- run $ intcode x
                return $ output === Just (999 + fromEnum (compare x 8))
