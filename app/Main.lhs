# [Advent of Code 2019](https://adventofcode.com/2019)
### my answers in [Haskell](https://www.haskell.org/) (see also [Kotlin branch](https://github.com/ephemient/aoc2019/tree/kt), [Python branch](https://github.com/ephemient/aoc2019/tree/py), and [Rust branch](https://github.com/ephemient/aoc2019/tree/rs))

This project builds with [The Haskell Tool Stack](https://haskellstack.org/).

Setup:

```sh
curl -sSL https://get.haskellstack.org/ | sh -s -
stack setup
```

Run the [Hspec](https://hspec.github.io/) test suite:

```sh
stack test aoc2019:test:aoc2019-test
```

Run [criterion](http://www.serpentine.com/criterion/) benchmarks ([results online](https://ephemient.github.io/aoc2019/aoc2019-bench.html)):

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
## [Day 9: Sensor Boost](/src/Day9.hs)
```haskell
import Day9 (day9a, day9b)
```
## [Day 10: Monitoring Station](/src/Day10.hs)
```haskell
import Day10 (day10a, day10b)
```
## [Day 11: Space Police](/src/Day11.hs)
```haskell
import Day11 (day11a, day11b)
```
## [Day 12: The N-Body Problem](/src/Day12.hs)
```haskell
import Day12 (day12a, day12b)
```
## [Day 13: Care Package](/src/Day13.hs)
```haskell
import Day13 (day13a, day13b)
```
## [Day 14: Space Stoichiometry](/src/Day14.hs)
```haskell
import Day14 (day14a, day14b)
```
## [Day 15: Oxygen System](/src/Day15.hs)
```haskell
import Day15 (day15a, day15b)
```
## [Day 16: Flawed Frequency Transmission](/src/Day16.hs)
```haskell
import Day16 (day16a, day16b)
```
## [Day 17: Set and Forget](/src/Day17.hs)
```haskell
import Day17 (day17a, day17b)
```
## [Day 18: Many-Worlds Interpretation](/src/Day18.hs)
```haskell
import Day18 (day18a, day18b)
```
## [Day 19: Tractor Beam](/src/Day19.hs)
```haskell
import Day19 (day19a, day19b)
```
## [Day 20: Donut Maze](/src/Day20.hs)
```haskell
import Day20 (day20a, day20b)
```
## [Day 21: Springboard Adventure](/src/Day21.hs)
```haskell
import Day21 (day21a, day21b)
```
## [Day 22: Slam Shuffle](/src/Day22.hs)
```haskell
import Day22 (day22a, day22b)
```
## [Day 23: Category Six](/src/Day23.hs)
```haskell
import Day23 (day23a, day23b)
```
## [Day 24: Planet of Discord](/src/Day24.hs)
```haskell
import Day24 (day24a, day24b)
```
## [Day 25: Cryostasis](/src/Day25.hs)
```haskell
import Day25 (day25)
```

---

```haskell
import Control.Monad ((<=<), when)
import Data.Maybe (mapMaybe)
import Paths_aoc2019 (getDataFileName)
import System.Environment (getArgs)
import Text.Megaparsec (ParseErrorBundle, ShowErrorComponent, Stream, errorBundlePretty)
import Text.Read (readMaybe)

getDayInput :: Int -> IO String
getDayInput i = getDataFileName ("day" ++ show i ++ ".txt") >>= readFile

justOrFail :: (Monad m) => Maybe a -> m a
justOrFail = maybe (fail "(⊥)") return

rightOrFail :: (ShowErrorComponent e, Stream s, Monad m) =>
    Either (ParseErrorBundle s e) a -> m a
rightOrFail = either (fail . errorBundlePretty) return

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
    run 2 (print <=< justOrFail <=< rightOrFail) [day2a, day2b]
    run 3 (print <=< justOrFail <=< rightOrFail) [day3a, day3b]
    run 4 (print <=< rightOrFail) [day4a, day4b]
    run 5 (print <=< justOrFail <=< rightOrFail) [day5a, day5b]
    run 6 (print <=< justOrFail) [day6a, day6b]
    run 7 (print <=< justOrFail <=< rightOrFail) [day7a, day7b]
    run 8 putStrLn [show . day8a 25 6, day8b 25 6]
    run 9 (print <=< justOrFail <=< rightOrFail) [day9a, day9b]
    run 10 print [day10a, (!! 199) . day10b]
    run 11 (putStrLn <=< rightOrFail) [fmap show . day11a, day11b]
    run 12 (print <=< rightOrFail) [fmap (!! 1000) . day12a, day12b]
    run 13 (print <=< justOrFail <=< rightOrFail) [fmap Just . day13a, day13b]
    run 14 (print <=< rightOrFail) [day14a, day14b]
    run 15 (print <=< justOrFail <=< rightOrFail) [day15a, day15b]
    run 16 putStrLn [(!! 100) . day16a, day16b]
    run 17 (print <=< justOrFail <=< rightOrFail) [fmap Just . day17a, day17b]
    run 18 (print <=< justOrFail) [day18a, day18b]
    run 19 (print <=< rightOrFail) [day19a, day19b]
    run 20 (print <=< justOrFail) [day20a, day20b]
    run 21 (print <=< justOrFail <=< rightOrFail) [day21a, day21b]
    run 22 (print <=< rightOrFail) [day22a, day22b]
    run 23 (print <=< justOrFail <=< rightOrFail) [day23a, day23b]
    run 24 (print <=< justOrFail) [day24a, Just . (!! 200) . day24b]
    run 25 (print <=< rightOrFail) [day25]
```
