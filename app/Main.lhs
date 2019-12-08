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
## [Day 2: 1202 Program Alarm](/src/Day2.hs)
```haskell
import Day2 (day2a, day2b)
```
## [Day 3: Crossed Wires](/src/Day3.hs)
```haskell
import Day3 (day3a, day3b)
```
## [Day 4: Secure Container](/src/Day4.hs)
```haskell
import Day4 (day4a, day4b)
```
## [Day 5: Sunny with a Chance of Asteroids](/src/Day5.hs)
```haskell
import Day5 (day5a, day5b)
```
## [Day 6: Universal Orbit Map](/src/Day6.hs)
```haskell
import Day6 (day6a, day6b)
```
## [Day 7: Amplification Circuit](/src/Day7.hs)
```haskell
import Day7 (day7a, day7b)
```
## [Day 8: Space Image Format](/src/Day8.hs)
```haskell
import Day8 (day8a, day8b)
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

maybeBottom :: (a -> String) -> Maybe a -> String
maybeBottom = maybe "(⊥)"

showError :: (Show a) => (b -> String) -> Either a b -> String
showError = either (\err -> "(" ++ show err ++ ")")

run :: Int -> (a -> IO ()) -> [String -> a] -> IO ()
run day showIO funcs = do
    days <- mapMaybe readMaybe <$> getArgs
    when (null days || day `elem` days) $ do
    putStrLn $ "Day " ++ show day
    contents <- getDayInput day
    mapM_ (showIO . ($ contents)) funcs
    putStrLn ""

main :: IO ()
main = do
    run 1 print [day1a, day1b]
    run 2 print [day2a, day2b]
    run 3 (putStrLn . showError (maybeBottom show)) [day3a, day3b]
    run 4 (putStrLn . showError show) [day4a, day4b]
    run 5 (putStrLn . maybeBottom show) [day5a, day5b]
    run 6 (putStrLn . maybeBottom show) [day6a, day6b]
    run 7 (putStrLn . showError (maybeBottom show)) [day7a, day7b]
    run 8 putStrLn [show . day8a 25 6, day8b 25 6]
```
