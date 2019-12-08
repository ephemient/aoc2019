package io.github.ephemient.aoc2019

class Day8(private val lines: List<String>, private val width: Int = 25, private val height: Int = 6) {
    fun part1(): Int? = lines.firstOrNull()
        ?.chunked(width * height)
        ?.minBy { it.count { it == '0' } }
        ?.let { it.count { it == '1' } * it.count { it == '2' } }

    fun part2(): String? = lines.firstOrNull()
        ?.map { it - '0' }
        ?.chunked(width * height)
        ?.foldRight<List<Int>, List<Int>?>(null) { s, acc ->
            acc?.zip(s) { a, b -> if (b == 2) a else b } ?: s
        }
        ?.chunked(width)
        ?.joinToString("\n") {
            it.joinToString("", transform = OUTPUT::get)
        }

    companion object {
        private val OUTPUT = listOf("\u2592", "\u2593", "\u2591")
    }
}
