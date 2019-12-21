module Day21Spec (spec) where

import Data.Set (Set)
import qualified Data.Set as Set (delete, fromDistinctAscList, insert, member)
import Day21 (Command(..), Register(..), programA, programB)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess, prop)
import Test.QuickCheck ((==>))

isSurvivable :: [Bool] -> Bool
isSurvivable [] = True
isSurvivable (False:_) = False
isSurvivable (_:rest) = isSurvivable (drop 3 rest) || isSurvivable rest

survives :: [Command Register] -> [Bool] -> Bool
survives program = survives' where
    survives' [] = True
    survives' (False:_) = False
    survives' (_:rest) = survives' $
        if go program $ toRegs rest then drop 3 rest else rest
    toRegs input = Set.fromDistinctAscList . map fst . filter snd .
        zip [A .. I] $ input ++ repeat True
    go (WALK:_) regs = Set.member J regs
    go (RUN:_) regs = Set.member J regs
    go (AND x y : commands) regs = go commands $
        if Set.member x regs then regs else Set.delete y regs
    go (OR x y : commands) regs = go commands $
        if Set.member x regs then Set.insert y regs else regs
    go (NOT x y : commands) regs = go commands $
        if Set.member x regs then Set.delete y regs else Set.insert y regs
    go _ _ = False

writesToReadOnlyRegister :: Command Register -> Bool
writesToReadOnlyRegister (AND _ J) = False
writesToReadOnlyRegister (AND _ T) = False
writesToReadOnlyRegister (OR _ J) = False
writesToReadOnlyRegister (OR _ T) = False
writesToReadOnlyRegister (NOT _ J) = False
writesToReadOnlyRegister (NOT _ T) = False
writesToReadOnlyRegister WALK = False
writesToReadOnlyRegister RUN = False
writesToReadOnlyRegister _ = True

extendedRegisters :: Set Register
extendedRegisters = Set.fromDistinctAscList [E .. I]

isExtended :: Command Register -> Bool
isExtended (AND r _) = Set.member r extendedRegisters
isExtended (OR r _) = Set.member r extendedRegisters
isExtended (NOT r _) = Set.member r extendedRegisters
isExtended RUN = True
isExtended _ = False

spec :: Spec
spec = do
    describe "part 1" $ do
        it "does not write to read only registers" $
            programA `shouldSatisfy` not . any writesToReadOnlyRegister
        it "does not use extended sensor mode" $
            programA `shouldSatisfy` not . any isExtended
        modifyMaxSize (const 8) $ modifyMaxSuccess (* 10) $
            prop "survives" $ \input ->
                isSurvivable (True:input) ==> survives programA (True:input)
    describe "part 2" $ do
        it "does not write to read only registers" $
            programB `shouldSatisfy` not . any writesToReadOnlyRegister
        modifyMaxSize (const 12) $ modifyMaxSuccess (* 100) $
            prop "survives" $ \input ->
                isSurvivable (True:input) ==> survives programB (True:input)
