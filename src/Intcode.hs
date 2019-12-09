{-# LANGUAGE LambdaCase, NamedFieldPuns, RecordWildCards #-}
module Intcode (Memory(..), run) where

data Memory m e i = Memory { readMem :: i -> m e , writeMem :: i -> e -> m () }

data Context m e i a = Context
  { next :: [e] -> i -> i -> m a
  , output :: e -> [e] -> i -> i -> m a
  , terminate :: i -> i -> m a
  }

run :: (Monad m, Integral e, Num i) => Memory m e i -> [e] -> m [e]
run memory = flip next 0 `flip` 0 where
    next = step memory Context {..}
    output e input base ip = (e:) <$> next input base ip
    terminate _ _ = return []

step :: (Monad m, Integral e, Num i) =>
    Memory m e i -> Context m e i a -> [e] -> i -> i -> m a
step Memory {..} Context {..} input base ip = do
    op <- readMem ip
    let arg n = case op `quot` 10 ^ (n + 1 :: Int) `rem` 10 of
            0 -> fromIntegral <$> readMem (ip + fromIntegral n)
            1 -> return $ ip + fromIntegral n
            2 -> (+) base . fromIntegral <$> readMem (ip + fromIntegral n)
            _ -> fail "bad mode"
        getArg n = arg n >>= readMem
        putArg n v = arg n >>= flip writeMem v
        binOp f = do
            f <$> getArg 1 <*> getArg 2 >>= putArg 3
            next input base $ ip + 4
        jmp p = p <$> getArg 1 >>= \case
            False -> next input base $ ip + 3
            True -> getArg 2 >>= next input base . fromIntegral
    case op `rem` 100 of
        1 -> binOp (+)
        2 -> binOp (*)
        3 | i:input' <- input -> putArg 1 i >> next input' base (ip + 2)
          | otherwise -> fail "no input"
        4 -> getArg 1 >>= flip output input `flip` base `flip` (ip + 2)
        5 -> jmp (/= 0)
        6 -> jmp (== 0)
        7 -> binOp $ \x y -> if x < y then 1 else 0
        8 -> binOp $ \x y -> if x == y then 1 else 0
        9 -> getArg 1 >>= (next input `flip` (ip + 2)) . (+) base . fromIntegral
        99 -> terminate base ip
        _ -> fail "bad opcode"
