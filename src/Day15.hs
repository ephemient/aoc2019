{-|
Module:         Day15
Description:    <https://adventofcode.com/2019/day/15 Day 15: Oxygen System>
-}
{-# LANGUAGE FlexibleContexts, TupleSections, TypeApplications, ViewPatterns #-}
module Day15 (day15a, day15b) where

import Control.Monad (filterM, when)
import Control.Monad.Cont (callCC, runCont)
import Control.Monad.RWS (evalRWS, evalRWST)
import Control.Monad.State (get, gets, modify, put, runState, runStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (tell)
import Data.Functor (($>))
import Data.Functor.Identity (Identity(..))
import Data.Heap (FstMinPolicy, Heap, HeapItem(..))
import qualified Data.Heap as Heap (insert, singleton, view)
import Data.List (foldl')
import Data.Maybe (catMaybes)
import Data.Semigroup (Last(..))
import Data.Set (Set)
import qualified Data.Set as Set (delete, fromList, insert, member, singleton)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as Vector (fromList)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Data.Void (Void)
import Intcode (State(State), getOutput, runIntcodeT)
import Intcode.Lazy (memory)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, parse, sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (Vector v e, Integral e, MonadParsec err String m) => m (v e)
parser = Vector.fromList <$> signed (return ()) decimal `sepBy` char ','

advance :: (Vector v e, Integral e, Monad m) =>
    (v e, e, e) -> e -> m (Maybe e, (v e, e, e))
advance (mem, base, ip) input = do
    ((State _ base' ip', output), mem') <- flip runStateT mem $
        runIntcodeT getOutput memory $ State (return input) base ip
    return (output, (mem', base', ip'))

bfsWithM :: (HeapItem pol item, Traversable t, Monad m) =>
    (item -> m (t item)) -> Heap pol item -> m ()
bfsWithM f = bfsWithM' where
    bfsWithM' (Heap.view -> Just (item, queue)) = f item >>=
        bfsWithM' . foldl' (flip Heap.insert) queue
    bfsWithM' _ = return ()

neighbors :: (Num e, Num a, Ord a) => (a, a) -> [((a, a), e)]
neighbors (x, y) =
    [((x, y - 1), 1), ((x, y + 1), 2), ((x - 1, y), 3), ((x + 1, y), 4)]

findOxygen :: (Vector v e, Integral e, Num a, Ord a, Monad m) =>
    (a -> (a, a) -> m b) -> v e -> m (Set (a, a))
findOxygen f mem0 = Set.fromList . snd <$> evalRWST
    (bfsWithM step $ Heap.singleton @FstMinPolicy (1, ((0, 0), (mem0, 0, 0))))
    () (Set.singleton (0, 0)) where
    step (d, (pos, state)) = tell [pos] >>
        catMaybes <$> mapM (tryStep d state) (neighbors pos)
    tryStep d state (pos, direction) = do
        visited <- get
        if pos `Set.member` visited then return Nothing else
            put (Set.insert pos visited) >>
            case runIdentity $ advance state direction of
                (Just 0, _) -> return Nothing
                (Just 1, state') -> return $ Just (d + 1, (pos, state'))
                (Just 2, state') ->
                    lift (f d pos) $> Just (d + 1, (pos, state'))
                _ -> error "unexpected response or termination"

flood :: (Num a, Ord a) => Set (a, a) -> (a, a) -> Maybe Int
flood pass start = fmap getLast $ snd $ evalRWS
    (bfsWithM step $ Heap.singleton @FstMinPolicy (0, start)) () $
    Set.delete start pass where
    step (d, pos) = tell (Just $ Last d) >>
        fmap (d + 1,) <$> filterM visit (fst <$> neighbors @Int pos)
    visit pos = do
        unvisited <- gets $ Set.member pos
        when unvisited $ modify $ Set.delete pos
        return unvisited

day15a :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day15a input = do
    mem0 <- parse (parser @Unboxed.Vector @Int) "" input
    return $ (`runCont` id) $ callCC $ \exit ->
        findOxygen (const . exit . Just) mem0 $> Nothing

day15b :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day15b input = do
    mem0 <- parse (parser @Unboxed.Vector @Int) "" input
    let (pass, oxygen) = runState (findOxygen (const $ put . Just) mem0) Nothing
    return $ oxygen >>= flood @Int pass
