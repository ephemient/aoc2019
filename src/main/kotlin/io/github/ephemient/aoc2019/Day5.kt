package io.github.ephemient.aoc2019

class Day5(lines: List<String>) {
    private val ints: List<Int> =
        lines.first().splitToSequence(",").map { it.toInt() }.toList()

    fun part1(): Int? = Intcode(ints.toIntArray()).runBlocking(listOf(1)).lastOrNull()

    fun part2(): Int? = Intcode(ints.toIntArray()).runBlocking(listOf(5)).lastOrNull()
}
