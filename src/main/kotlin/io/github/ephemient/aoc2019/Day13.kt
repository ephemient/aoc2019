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

    fun part2(): Long? = runBlocking {
        var score: Long? = null
        var ball: Long? = null
        var paddle: Long? = null
        var x = 0L
        var y = 0L
        var state = State.PENDING_X
        Intcode(ints.toMutableList().also { it[0] = 2 }).runAsync(
            input = { compareValues(ball, paddle).sign.toLong() },
            output = { output ->
                when (state) {
                    State.PENDING_X -> {
                        x = output
                        state = State.PENDING_Y
                    }
                    State.PENDING_Y -> {
                        y = output
                        state = State.PENDING_OUTPUT
                    }
                    State.PENDING_OUTPUT -> {
                        when {
                            x == -1L && y == 0L -> score = output
                            output == 3L -> paddle = x
                            output == 4L -> ball = x
                        }
                        state = State.PENDING_X
                    }
                }
            }
        )
        return@runBlocking score
    }

    private enum class State {
        PENDING_X,
        PENDING_Y,
        PENDING_OUTPUT,
    }
}
