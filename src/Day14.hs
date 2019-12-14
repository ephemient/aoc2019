{-|
Module:         Day14
Description:    <https://adventofcode.com/2019/day/14 Day 14: Space Stoichiometry>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module Day14 (day14a, day14b) where

import Data.LinearProgram (GLPOpts(msgLev), MsgLev(MsgOff), ReturnCode(Success), VarKind(IntVar), equalTo, execLPM, geqTo, glpSolveVars, mipDefaults, setObjective, setVarKind, varGeq)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map ((!), elems, fromList, fromSet, insertWith, keysSet, lookup, withoutKeys)
import qualified Data.Set as Set (fromList, unions)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, errorBundlePretty, parse, sepBy, sepEndBy, some)
import Text.Megaparsec.Char (alphaNumChar, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (MonadParsec e String m) => m ([String], Map String (Map String Int))
parser = do
    let formula = do
            lhs <- Map.fromList <$> pair `sepBy` string ", "
            (k, v) <- string " => " *> pair
            return $ Map.insertWith (+) k v $ negate <$> lhs
        pair = flip (,) <$> (decimal <* space) <*> some alphaNumChar
    formulae <- zipWith ((,) . (++) "rule" . show @Int) [1..] <$>
        formula `sepEndBy` newline
    let coefficients key = Map.fromList
            [(k, v) | (k, Just v) <- fmap (Map.lookup key) <$> formulae]
    return
      ( fst <$> formulae
      , Map.fromSet coefficients $ Set.unions $ Map.keysSet . snd <$> formulae
      )

glpOpts :: GLPOpts
glpOpts = mipDefaults {msgLev = MsgOff}

day14a :: String -> IO Int
day14a input = do
    (vars, equations) <- either (fail . errorBundlePretty) return $
        parse @Void parser "" input
    result <- glpSolveVars glpOpts $ execLPM $ do
        mapM_ (`varGeq` 0) vars
        mapM_ (`setVarKind` IntVar) vars
        equations Map.! "FUEL" `equalTo` 1
        mapM_ (`geqTo` 0) $ Map.elems $
            equations `Map.withoutKeys` Set.fromList ["FUEL", "ORE"]
        setObjective $ equations Map.! "ORE"
    case result of
        (Success, Just (objective, _)) -> do
            return $ round (-objective)
        _ -> fail $ show result

day14b :: String -> IO Int
day14b input = do
    (vars, equations) <- either (fail . errorBundlePretty) return $
        parse @Void parser "" input
    result <- glpSolveVars glpOpts $ execLPM $ do
        mapM_ (`varGeq` 0) vars
        mapM_ (`setVarKind` IntVar) vars
        equations Map.! "ORE" `geqTo` (-1000000000000)
        mapM_ (`geqTo` 0) $ Map.elems $
            equations `Map.withoutKeys` Set.fromList ["FUEL", "ORE"]
        setObjective $ equations Map.! "FUEL"
    case result of
        (Success, Just (objective, _)) -> do
            return $ round objective
        _ -> fail $ show result
