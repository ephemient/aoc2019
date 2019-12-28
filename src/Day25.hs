{-|
Module:         Day25
Description:    <https://adventofcode.com/2019/day/25 Day 25: Cryostasis>
-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, FlexibleContexts, NamedFieldPuns, RecordWildCards, TupleSections, TypeApplications, ViewPatterns #-}
module Day25 (day25) where

import Common (bfsM)
import Control.Applicative hiding (many, some)
import Control.DeepSeq (NFData(..))
import Control.Monad (forM_, unless, when)
import Control.Monad.Cont (callCC, runCont)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.ST (runST)
import Control.Monad.State (MonadState, StateT, evalStateT, get, modify, put)
import Control.Monad.Trans (lift)
import Data.Char (isSpace)
import Data.Containers.ListUtils (nubOrd)
import Data.Functor (($>), void)
import Data.List (dropWhileEnd, isPrefixOf)
import Data.Map.Lazy (Map, (!?))
import qualified Data.Map.Lazy as Map (assocs, empty, insertWith, union, singleton)
import Data.Maybe (isNothing)
import Data.Set (Set, (\\))
import qualified Data.Set as Set (delete, empty, insert, lookupMin, member, powerSet)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Debug.Trace (traceM)
import GHC.Generics (Generic)
import Intcode (IntcodeT, runIntcodeT)
import qualified Intcode (State(..), setInput)
import Intcode.Char (getOutputLine, makeStringInput)
import Intcode.Diff (DiffableMemory, checkDiff, getDiffs, undoDiffs, wrapMemory)
import qualified Intcode.Diff as Diff (mem)
import Intcode.Vector (memory, parser)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, ParsecT, ShowErrorComponent(..), anySingle, between, customFailure, getInput, getParserState, notFollowedBy, optional, parse, parseErrorPretty, runParserT, setInput, setParserState, skipMany, skipSome, skipMany, skipManyTill, some, try, withRecovery)
import Text.Megaparsec.Char (newline, printChar, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

data SantaError = Loop | NoPath | NoTarget | NoCombination
  deriving (Eq, Generic, NFData, Ord)
instance ShowErrorComponent SantaError where
    showErrorComponent Loop = "<<loop>>"
    showErrorComponent NoPath = "no path"
    showErrorComponent NoTarget = "no target"
    showErrorComponent NoCombination = "no combination"

data RPG room door item = RPG
  { room :: room
  , allItems :: Set item
  , blacklist :: Set item
  , explored :: Map room (Map door room)
  , unexplored :: [(room, door)]
  , test :: Maybe room
  } deriving (Show)

data Unlock room door item = Unlock
  { room :: room
  , heldItems :: Set item
  , queue :: Set (Set item)
  , explored :: Map room (Map door room)
  , test :: room
  } deriving (Show)

addStanzaFromIntcode :: (PrimMonad m) =>
    DiffableMemory m Int -> ParsecT SantaError String (StateT (Intcode.State (IntcodeT Int m) Int) m) ()
addStanzaFromIntcode diffable = addStanzaFromIntcode' Set.empty where
    addStanzaFromIntcode' seen = do
        intcodeState@Intcode.State {base, ip} <- get
        diffs <- lift . lift $ getDiffs diffable
        let vmState = (base, ip, diffs)
        when (Set.member vmState seen) $ customFailure Loop
        (intcodeState', maybeLine) <- lift . lift $
            runIntcodeT getOutputLine (Diff.mem diffable) intcodeState
        put intcodeState'
        maybe (return ()) `flip` maybeLine $ \line -> do
            traceM $ dropWhileEnd isSpace line
            getInput >>= setInput . (++ line)
            unless ("Command?" `isPrefixOf` line) .
                addStanzaFromIntcode' $ Set.insert vmState seen

readTitle :: (MonadParsec err String m) => m String
readTitle = do
    space
    title <- between (string "== ") newline $ some printChar
    skipMany . try $ skipSome printChar >> newline
    return title 

readDoors :: (MonadParsec err String m) => m [String]
readDoors = space *> string "Doors here lead:" *> newline *>
    some (between (string "- ") newline $ some printChar)

readItems :: (MonadParsec err String m) => m [String]
readItems = space *> string "Items here:" *> newline *>
    some (between (string "- ") newline $ some printChar)

readCommand :: (MonadParsec err String m) => m ()
readCommand = space >> string "Command?" >> void newline

setCommand :: (MonadState (Intcode.State (IntcodeT Int m1) Int) m, Monad m1) =>
    String -> m ()
setCommand input = do
    traceM input
    modify $ \s -> s {Intcode.input = makeStringInput nl input}
  where nl = Intcode.setInput (fail "no more input") $> 10

startRPG :: (PrimMonad m) =>
    DiffableMemory m Int -> ParsecT SantaError String (StateT (Intcode.State (IntcodeT Int m) Int) m) Int
startRPG diffable = do
    addStanzaFromIntcode diffable
    room <- readTitle
    doors <- try readDoors <|> pure []
    items <- try readItems <|> pure []
    readCommand
    pickupItems diffable doors items RPG
      { allItems = Set.empty
      , blacklist = Set.empty
      , explored = Map.empty
      , unexplored = []
      , test = Nothing
      , ..
      }

pickupItems :: (PrimMonad m) =>
    DiffableMemory m Int -> [String] -> [String] -> RPG String String String -> ParsecT SantaError String (StateT (Intcode.State (IntcodeT Int m) Int) m) Int
pickupItems diffable doors (item:items) rpg@RPG {allItems, blacklist}
  | Set.member item blacklist = pickupItems diffable doors items rpg
  | otherwise = do
    lift . lift . void $ checkDiff diffable
    parserState <- getParserState
    intcodeState <- get
    let takeItem = do
            setCommand $ "take " ++ item
            addStanzaFromIntcode diffable
            space >> string ("You take the " ++ item ++ ".") >> void newline
            readCommand
    ok <- withRecovery undo $ do
        takeItem
        setCommand $ head doors
        addStanzaFromIntcode diffable
        readTitle $> True
    traceM "<<undo>>"
    lift . lift $ undoDiffs diffable
    setParserState parserState
    put intcodeState
    if ok
    then takeItem >> pickupItems diffable doors items
            rpg {allItems = Set.insert item allItems}
    else pickupItems diffable doors items
            rpg {blacklist = Set.insert item blacklist}
  where undo err = traceM (parseErrorPretty err) $> False
pickupItems diffable doors _ rpg@RPG{room, unexplored} =
    explore diffable rpg {unexplored = nubOrd $ map (room,) doors ++ unexplored}

explore :: (PrimMonad m) =>
    DiffableMemory m Int -> RPG String String String -> ParsecT SantaError String (StateT (Intcode.State (IntcodeT Int m) Int) m) Int
explore diffable rpg@RPG
    {room = curRoom , unexplored = (lastRoom, nextDoor) : unexplored , .. } = do
    True <- navigate diffable explored curRoom lastRoom
    setCommand nextDoor
    addStanzaFromIntcode diffable
    room <- readTitle
    doors <- try readDoors <|> pure []
    items <- try readItems <|> pure []
    let isUnexplored door = isNothing $ explored !? room >>= (!? door)
    try santa <|> do
        (room', test') <- try readCommand $> (room, test) <|> do
            space
            skipMany $ skipSome printChar >> newline
            room' <- readTitle
            void . optional $ try readDoors
            void . optional $ try readDoors
            readCommand
            return (room', Just room)
        pickupItems diffable (filter isUnexplored doors) items rpg
          { room = room'
          , explored = Map.insertWith Map.union lastRoom
                (Map.singleton nextDoor room) explored
          , unexplored
          , test = test'
          }
explore diffable RPG{test = Just test, ..} = guess diffable
    Unlock {heldItems = allItems, queue = Set.powerSet allItems, ..}
explore _ _ = customFailure NoTarget

guess :: (PrimMonad m) =>
    DiffableMemory m Int -> Unlock String String String -> ParsecT SantaError String (StateT (Intcode.State (IntcodeT Int m) Int) m) Int
guess diffable unlock@Unlock {..} = do
    False <- navigate diffable explored room test
    try santa <|> case Set.delete heldItems queue of
        queue'@(Set.lookupMin -> Just heldItems') -> do
            space
            skipMany $ skipSome printChar >> newline
            room' <- readTitle
            void . optional $ try readDoors
            void . optional $ try readItems
            readCommand
            forM_ (heldItems \\ heldItems') $ \item -> do
                setCommand $ "drop " ++ item
                addStanzaFromIntcode diffable
                space
                void $ string ("You drop the " ++ item ++ ".")
                void newline
                readCommand
            forM_ (heldItems' \\ heldItems) $ \item -> do
                setCommand $ "take " ++ item
                addStanzaFromIntcode diffable
                space
                void $ string ("You take the " ++ item ++ ".")
                void newline
                readCommand
            guess diffable unlock
                {room = room' , heldItems = heldItems' , queue = queue'}
        _ -> customFailure NoCombination

santa :: (MonadParsec err String m) => m Int
santa = do
    space
    skipMany $ notFollowedBy (string "Santa ") >> skipSome printChar >> newline
    string "Santa " >> skipManyTill anySingle decimal

navigate :: (PrimMonad m) =>
    DiffableMemory m Int -> Map String (Map String String) -> String -> String -> ParsecT SantaError String (StateT (Intcode.State (IntcodeT Int m) Int) m) Bool
navigate diffable rooms current goal
  | Just path <- flip runCont id $ callCC $ \exit ->
        bfsM fst (go exit) (current, []) $> Nothing = walk path
  | otherwise = customFailure NoPath
  where
    go exit _ (room, path)
      | room == goal = exit . Just $ reverse path
      | Just doors <- rooms !? room = return
        [(room', door:path) | (door, room') <- Map.assocs doors]
      | otherwise = return []
    walk [] = return True
    walk (door:path) = do
        setCommand door
        addStanzaFromIntcode diffable
        void readTitle
        void readDoors
        void . optional $ try readItems
        try readCommand *> walk path <|> pure False

day25 :: String -> Either (ParseErrorBundle String SantaError) Int
day25 input = do
    mem0 <- parse (parser @Unboxed.Vector @Int) "" input
    runST $ do
        diffable <- memory mem0 >>= wrapMemory
        evalStateT (runParserT (startRPG diffable) "" "")
            Intcode.State {input = fail "no input", base = 0, ip = 0}
