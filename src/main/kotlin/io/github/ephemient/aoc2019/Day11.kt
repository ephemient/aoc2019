package io.github.ephemient.aoc2019

import kotlinx.coroutines.runBlocking

class Day11(private val lines: List<String>) {
    private val ints: List<Long> =
        lines.first().splitToSequence(",").map { it.toLong() }.toList()

    @Suppress("LoopWithTooManyJumpStatements")
    private suspend fun walk(start: Boolean): Map<Pair<Int, Int>, Boolean> {
        val grid = mutableMapOf(0 to 0 to start)
        var x = 0
        var y = 0
        var dx = 0
        var dy = 1
        val vm = Intcode(ints.toMutableList())
        val input = suspend { if (grid[x to y] == true) 1L else 0L }
        while (true) {
            grid[x to y] = (vm.getOutput(input) ?: break) != 0L
            if ((vm.getOutput(input) ?: break) != 0L) {
                dy = -dx.also { dx = dy }
            } else {
                dx = -dy.also { dy = dx }
            }
            x += dx
            y += dy
        }
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
