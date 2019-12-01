# [Advent of Code 2019](https://adventofcode.com/2019)
### my answers in [Kotlin](https://www.kotlinlang.org/) (see also [Haskell branch](https://github.com/ephemient/aoc2019/tree/hs) and [Python branch](https://github.com/ephemient/aoc2019/tree/py))

This project builds with [Gradle](https://gradle.org/).

Run the [JUnit 5](https://junit.org/junit5/) test suite:

```sh
./gradlew test
```

Run [JMH](https://openjdk.java.net/projects/code-tools/jmh/) benchmarks:

```sh
./gradlew jmh
```

Print solutions for the inputs provided in local data files:

```sh
./gradlew run
```

Generate [Dokka](https://github.com/Kotlin/dokka) API documentation:

```sh
./gradlew dokka
```

Run [ktlint](https://ktlint.github.io/) Kotlin linter:

```sh
./gradlew lintKotlin
```

Check for newer versions of dependencies:

```sh
./gradlew dependencyUpdates -Drevision=release
```
