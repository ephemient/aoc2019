package io.github.ephemient.aoc2019

import kotlinx.coroutines.runBlocking

class Day11(private val lines: List<String>) {
    private val ints: List<Long> =
        lines.first().splitToSequence(",").map { it.toLong() }.toList()

    private suspend fun walk(start: Boolean): Map<Pair<Int, Int>, Boolean> {
        val grid = mutableMapOf(0 to 0 to start)
        var x = 0
        var y = 0
        var dx = 0
        var dy = 1
        var turning = false
        Intcode(ints.toMutableList()).runAsync(
            input = { if (grid[x to y] == true) 1 else 0 },
            output = { value ->
                if (turning) {
                    if (value == 0L) {
                        dx = -dy.also { dy = dx }
                    } else {
                        dy = -dx.also { dx = dy }
                    }
                    x += dx
                    y += dy
                    turning = false
                } else {
                    grid[x to y] = value != 0L
                    turning = true
                }
            }
        )
        return grid
    }

    fun part1(): Int = runBlocking { walk(false) }.count()

    fun part2(): String {
        val result = runBlocking { walk(true) }
            .mapNotNullTo(mutableSetOf<Pair<Int, Int>>()) { (key, value) -> key.takeIf { value } }
        val (xs, ys) = result.unzip()
        return (checkNotNull(ys.min())..checkNotNull(ys.max())).reversed().joinToString("\n") { y ->
            (checkNotNull(xs.min())..checkNotNull(xs.max())).joinToString("") { x ->
                if (x to y in result) "\u2593" else "\u2591"
            }
        }
    }
}
