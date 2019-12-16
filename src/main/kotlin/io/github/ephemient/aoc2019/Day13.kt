package io.github.ephemient.aoc2019

import kotlin.math.sign
import kotlinx.coroutines.runBlocking

class Day13(lines: List<String>) {
    private val ints: List<Long> =
        lines.first().splitToSequence(",").map { it.toLong() }.toList()

    fun part1(): Int = Intcode(ints.toMutableList())
        .runBlocking(emptyList())
        .chunked(3)
        .fold(mutableSetOf<Pair<Long, Long>>()) { acc, (x, y, z) ->
            if (z == 2L) {
                acc.add(x to y)
            } else {
                acc.remove(x to y)
            }
            acc
        }
        .size

    @Suppress("LoopWithTooManyJumpStatements")
    fun part2(): Long? = runBlocking {
        var score: Long? = null
        var ball: Long? = null
        var paddle: Long? = null
        val vm = Intcode(ints.toMutableList().also { it[0] = 2 })
        val input = suspend { compareValues(ball, paddle).sign.toLong() }
        while (true) {
            val x = vm.getOutput(input) ?: break
            val y = vm.getOutput(input) ?: break
            val value = vm.getOutput(input) ?: break
            when {
                x == -1L && y == 0L -> score = value
                value == 3L -> paddle = x
                value == 4L -> ball = x
            }
        }
        return@runBlocking score
    }
}
