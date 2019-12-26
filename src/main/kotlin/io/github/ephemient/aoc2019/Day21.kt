package io.github.ephemient.aoc2019

class Day21(lines: List<String>) {
    private val ints: List<Long> =
        lines.first().splitToSequence(",").map { it.toLong() }.toList()

    fun part1(): Long? = Intcode(ints.toMutableList())
        .runBlocking(
            """
            OR A T
            AND B T
            AND C T
            NOT T J
            AND D J
            WALK
            """.trimIndent().map(Char::toLong) + 10
        )
        .firstOrNull { it > 255 }

    fun part2(): Long? = Intcode(ints.toMutableList())
        .runBlocking(
            """
            OR A T
            AND B T
            AND C T
            NOT T J
            AND D J
            NOT J T
            OR E T
            OR H T
            AND T J
            RUN
            """.trimIndent().map(Char::toLong) + 10
        )
        .firstOrNull { it > 255 }
}
