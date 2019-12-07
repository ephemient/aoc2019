module Main (main) where

import Control.Monad (forM_)
import qualified Data.IntMap as IntMap ((!), insertWith, keys, singleton, size, unionWith)
import qualified Data.IntSet as IntSet (delete, elems, empty, singleton, union)
import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import System.Random (randomIO)
import Text.Printf (printf)

unionAllWith :: (Monad m) => (Int -> m Int) -> (a -> a -> m a) -> [a] -> m a
unionAllWith _ _ [] = fail "empty"
unionAllWith pick add xs = unionAll xs where
    unionAll [x] = return x
    unionAll ys = do
        let n = length ys
        i <- pick $ n - 1
        j <- pick $ n - 1
        let i' = min i j
            j' = max i j - i'
            (before, x:ys') = splitAt i' ys
            (mid, y:after) = splitAt j' ys'
        x' <- add x y
        unionAll $ x' : (before ++ mid ++ after)

main :: IO ()
main = do
    size <- maybe 20 read . listToMaybe <$> getArgs
    let pick n = (`mod` n) <$> randomIO
        add m1 m2 = do
            i <- (IntMap.keys m1 !!) <$> pick (IntMap.size m1)
            j <- (IntMap.keys m2 !!) <$> pick (IntMap.size m2)
            return $ IntMap.insertWith IntSet.union i (IntSet.singleton j) $
                IntMap.insertWith IntSet.union j (IntSet.singleton i) $
                IntMap.unionWith IntSet.union m1 m2
    let name 0 = "COM"
        name 1 = "SAN"
        name 2 = "YOU"
        name n = printf "%03d" n
        walk i m = forM_ (IntSet.elems $ m IntMap.! i) $ \j -> do
            putStrLn $ name i ++ ")" ++ name j
            walk j $ IntSet.delete i <$> m
    result <- unionAllWith pick add $
        map (`IntMap.singleton` IntSet.empty) [0..size]
    walk 0 result
