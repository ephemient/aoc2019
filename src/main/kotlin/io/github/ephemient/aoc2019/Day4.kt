package io.github.ephemient.aoc2019

class Day4(lines: List<String>) {
    private val lo: String
    private val hi: String
    init {
        val (lo, hi) = lines.first().split("-", limit = 2)
        check(lo.length == hi.length)
        this.lo = lo
        this.hi = hi
    }

    private fun range(): Iterable<CharArray> = sequence<CharArray> {
        var chars = lo.toCharArray()
        for (i in 0 until chars.lastIndex) {
            chars[i + 1] = maxOf(chars[i], chars[i + 1])
        }
        while (
            chars.withIndex().find { (i, c) -> c != hi[i] }?.let { (i, c) -> c > hi[i] } != true
        ) {
            yield(chars)
            val i = chars.indexOfLast { it < '9' }.takeIf { it >= 0 } ?: return@sequence
            chars.fill(chars[i] + 1, fromIndex = i)
        }
    }.asIterable()

    fun part1(): Int = range().count { chars ->
        (0 until chars.lastIndex).any { chars[it] == chars[it + 1] }
    }

    fun part2(): Int = range().count { chars ->
        (0 until chars.lastIndex).any {
            chars[it] == chars[it + 1] &&
                (it <= 0 || chars[it - 1] != chars[it]) &&
                (it + 1 >= chars.lastIndex || chars[it + 1] != chars[it + 2])
        }
    }
}
