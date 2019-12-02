# [Advent of Code 2019](https://adventofcode.com/2019)
### my answers in [Haskell](https://www.haskell.org/) (see also [Kotlin branch](https://github.com/ephemient/aoc2019/tree/kt), [Python branch](https://github.com/ephemient/aoc2019/tree/py), and [Rust branch](https://github.com/ephemient/aoc2019/tree/rs))

This project builds with [The Haskell Tool Stack](https://haskellstack.org/).

Setup:

```sh
curl -sSL https://get.haskellstack.org/ | sh -s -
stack setup
```

Run the [HSpec](https://hspec.github.io/) test suite:

```sh
stack test aoc2019:test:aoc2019-test
```

Run [criterion](http://www.serpentine.com/criterion/) benchmarks:

```sh
stack bench aoc2019:bench:aoc2019-bench
```

Print solutions for the inputs provided in local data files:

```sh
stack build aoc2019:exe:aoc2019-exe --exec aoc2019-exe
```

Generate [Haddock](https://www.haskell.org/haddock/) API documentation:

```sh
stack haddock aoc2019:lib
```

Run [hlint](https://github.com/ndmitchell/hlint) source code suggestions:

```sh
stack build hlint --exec 'hlint src test bench'
```

---

<!--
```haskell
{-# LANGUAGE NondecreasingIndentation #-}
module Main (main) where
```
-->

## [Day 1: The Tyranny of the Rocket Equation](/src/Day1.hs)
```haskell
import Day1 (day1a, day1b)
```

---

```haskell
import Control.Monad (when)
import Data.Maybe (mapMaybe)
import Paths_aoc2019 (getDataFileName)
import System.Environment (getArgs)
import Text.Read (readMaybe)

getDayInput :: Int -> IO String
getDayInput i = getDataFileName ("day" ++ show i ++ ".txt") >>= readFile

readDayInput :: (Read a) => Int -> IO a
readDayInput = fmap read . getDayInput

maybeBottom :: (a -> String) -> Maybe a -> String
maybeBottom = maybe "(⊥)"

showError :: (Show a) => (b -> String) -> Either a b -> String
showError = either (\err -> "(" ++ show err ++ ")")

run :: Int -> (Int -> IO a) -> (b -> IO ()) -> [a -> b] -> IO ()
run day readIO showIO funcs = do
    days <- mapMaybe readMaybe <$> getArgs
    when (null days || day `elem` days) $ do
    putStrLn $ "Day " ++ show day
    contents <- readIO day
    mapM_ (showIO . ($ contents)) funcs
    putStrLn ""

main :: IO ()
main = do
    run 1 getDayInput putStrLn [show . day1a, show . day1b]
```
