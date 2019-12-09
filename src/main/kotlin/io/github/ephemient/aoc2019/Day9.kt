package io.github.ephemient.aoc2019

class Day9(lines: List<String>) {
    private val ints: List<Long> =
        lines.first().splitToSequence(",").map { it.toLong() }.toList()

    fun part1(): Long? = Intcode(ints.toMutableList()).runBlocking(listOf(1)).lastOrNull()

    fun part2(): Long? = Intcode(ints.toMutableList()).runBlocking(listOf(2)).lastOrNull()
}
