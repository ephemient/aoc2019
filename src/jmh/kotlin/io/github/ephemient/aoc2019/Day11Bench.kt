package io.github.ephemient.aoc2019

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.State

@State(Scope.Thread)
open class Day11Bench {
    private lateinit var lines: List<String>

    @Setup
    fun prepare() {
        lines = javaClass.classLoader.getResourceAsStream("day11.txt").bufferedReader().readLines()
    }

    @Benchmark
    fun part1(): Int = Day11(lines).part1()

    @Benchmark
    fun part2(): String = Day11(lines).part2()
}
