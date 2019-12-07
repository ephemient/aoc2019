{-# LANGUAGE LambdaCase, NamedFieldPuns, RecordWildCards #-}
module Intcode (Memory(..), Context(..), run, runLast, step) where

import Control.Monad.State (execStateT, lift, put)

data Memory m e i = Memory { readMem :: i -> m e, writeMem :: i -> e -> m () }

data Context m e i a = Context
  { next :: [e] -> i -> m a
  , output :: e -> [e] -> i -> m a
  , terminate :: m a
  }

run :: (Monad m, Integral e, Num i) => Memory m e i -> [e] -> m [e]
run memory = flip next 0 where
    context@Context {next} = Context
      { next = step memory context
      , output = \e input ip -> (e:) <$> next input ip
      , terminate = return []
      }

runLast :: (Monad m, Integral e, Num i) => Memory m e i -> [e] -> m (Maybe e)
runLast Memory {..} = flip execStateT Nothing . flip next 0 where
    memory = Memory { readMem = lift . readMem, writeMem = (lift .) . writeMem }
    context@Context {next} = Context
      { next = step memory context
      , output = \e input ip -> put (Just e) >> next input ip
      , terminate = return ()
      }

step :: (Monad m, Integral e, Num i) =>
    Memory m e i -> Context m e i a -> [e] -> i -> m a
step Memory {..} Context {..} input ip = do
    op <- readMem ip
    let arg n
          | op `quot` 10 ^ (n + 1 :: Int) `rem` 10 == 0
          = fromIntegral <$> readMem (ip + fromIntegral n)
          | otherwise = return $ ip + fromIntegral n
        getArg n = arg n >>= readMem
        putArg n v = arg n >>= flip writeMem v
        binOp f = do
            f <$> getArg 1 <*> getArg 2 >>= putArg 3
            next input $ ip + 4
        jmp p = p <$> getArg 1 >>= \case
            False -> next input $ ip + 3
            True -> getArg 2 >>= next input . fromIntegral
    case op `rem` 100 of
        1 -> binOp (+)
        2 -> binOp (*)
        3 | i:input' <- input -> putArg 1 i >> next input' (ip + 2)
          | otherwise -> fail "no input"
        4 -> getArg 1 >>= flip output input `flip` (ip + 2)
        5 -> jmp (/= 0)
        6 -> jmp (== 0)
        7 -> binOp $ \x y -> if x < y then 1 else 0
        8 -> binOp $ \x y -> if x == y then 1 else 0
        99 -> terminate
        _ -> fail "bad opcode"
