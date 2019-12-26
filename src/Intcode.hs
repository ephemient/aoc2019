{-# LANGUAGE LambdaCase, NamedFieldPuns, TupleSections, RecordWildCards #-}
module Intcode (IntcodeT, Memory(..), State(..), evalIntcodeT, getOutput, getState, liftMemory, run, runIntcodeT, setInput) where

import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as Fail (fail)
import Control.Monad.Loops (unfoldM)
import Control.Monad.Fix (fix)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Functor (($>))

data Memory m e = Memory { readMem :: e -> m e , writeMem :: e -> e -> m () }

data State m e = State { input :: m e, base :: e, ip :: e }

newtype IntcodeT e m a = IntcodeT {
    runIntcodeT
        :: Memory m e
        -> State (IntcodeT e m) e
        -> m (State (IntcodeT e m) e, a)
}

liftMemory :: (MonadTrans t, Monad m, Monad (t m)) =>
    Memory m e -> Memory (t m) e
liftMemory Memory {..} =
    Memory { readMem = lift . readMem, writeMem = (.) lift . writeMem }

evalIntcodeT :: (Functor m, Num e) =>
    IntcodeT e m a -> Memory m e -> IntcodeT e m e -> m a
evalIntcodeT intcode mem input =
    snd <$> runIntcodeT intcode mem State { base = 0, ip = 0, .. }

instance (Functor m) => Functor (IntcodeT e m) where
    fmap f IntcodeT {..} = IntcodeT $ \mem s -> fmap f <$> runIntcodeT mem s

instance (Monad m) => Applicative (IntcodeT e m) where
    pure a = IntcodeT $ const $ pure . (, a)
    IntcodeT runF <*> IntcodeT runA = IntcodeT $ \mem s -> do
        (s', f) <- runF mem s
        (s'', a) <- runA mem s'
        pure (s'', f a)

instance (Monad m) => Monad (IntcodeT e m) where
    IntcodeT runA >>= f = IntcodeT $ \mem s -> do
        (s', a) <- runA mem s
        runIntcodeT (f a) mem s'

instance (MonadFail m) => MonadFail (IntcodeT e m) where
    fail msg = IntcodeT $ \_ _ -> Fail.fail msg

instance MonadTrans (IntcodeT e) where lift m = IntcodeT $ \_ s -> (s,) <$> m

getState :: (Monad m) => IntcodeT e m (State (IntcodeT e m) e)
getState = IntcodeT $ \_ s -> return (s, s)

setInput :: (Monad m) => IntcodeT e m e -> IntcodeT e m ()
setInput input = IntcodeT $ \_ s -> return (s {input}, ())

getOutput :: (Monad m, Integral e) => IntcodeT e m (Maybe e)
getOutput = IntcodeT $ \mem@Memory {..} -> fix $ \loop state@State {..} -> do
    op <- fromIntegral <$> readMem ip
    let arg n = case op `quot` 10 ^ (n + 1 :: Int) `rem` 10 :: Int of
            0 -> readMem (ip + fromIntegral n)
            1 -> return $ ip + fromIntegral n
            2 -> (+) base <$> readMem (ip + fromIntegral n)
            _ -> fail "bad mode"
        getArg n = arg n >>= readMem
        putArg n v = arg n >>= flip writeMem v
        binOp f = do
            f <$> getArg 1 <*> getArg 2 >>= putArg 3
            loop state {ip = ip + 4}
        jmp p = p <$> getArg 1 >>= \case
            False -> loop state {ip = ip + 3}
            True -> getArg 2 >>= \ip' -> loop state {ip = ip'}
    case op `rem` 100 of
        1 -> binOp (+)
        2 -> binOp (*)
        3 -> do (state', v) <- runIntcodeT input mem state
                putArg 1 v
                loop state' {ip = ip + 2}
        4 -> (state {ip = ip + 2},) . Just <$> getArg 1
        5 -> jmp (/= 0)
        6 -> jmp (== 0)
        7 -> binOp $ \x y -> if x < y then 1 else 0
        8 -> binOp $ \x y -> if x == y then 1 else 0
        9 -> getArg 1 >>= \v -> loop state {base = base + v, ip = ip + 2}
        99 -> return (state, Nothing)
        _ -> fail "bad opcode"

run :: (Monad m, Integral e) => Memory m e -> [e] -> m [e]
run mem = evalIntcodeT (unfoldM getOutput) mem . getInput where
    getInput (i:input) = setInput (getInput input) $> i
    getInput _ = fail "no input"
