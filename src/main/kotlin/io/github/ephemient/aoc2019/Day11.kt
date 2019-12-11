package io.github.ephemient.aoc2019

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.async
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.channels.ClosedReceiveChannelException
import kotlinx.coroutines.channels.ClosedSendChannelException
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.runBlocking

class Day11(private val lines: List<String>) {
    private val ints: List<Long> =
        lines.first().splitToSequence(",").map { it.toLong() }.toList()

    private suspend fun CoroutineScope.walk(start: Boolean): Map<Pair<Int, Int>, Boolean> =
        coroutineScope {
            val input = Channel<Long>(Channel.UNLIMITED)
            val output = Channel<Long>()
            val result = async {
                val grid = mutableMapOf(0 to 0 to start)
                var x = 0
                var y = 0
                var dx = 0
                var dy = 1
                try {
                    input.send(if (start) 1 else 0)
                    while (true) {
                        val color = output.receive()
                        grid[x to y] = color != 0L
                        val turn = output.receive()
                        if (turn == 0L) {
                            dx = -dy.also { dy = dx }
                        } else {
                            dy = -dx.also { dx = dy }
                        }
                        x += dx
                        y += dy
                        input.send(if (grid[x to y] == true) 1 else 0)
                    }
                } catch (_: ClosedReceiveChannelException) {
                } catch (_: ClosedSendChannelException) {
                }
                return@async grid
            }
            Intcode(ints.toMutableList()).runAsync(input, output)
            input.close()
            output.close()
            return@coroutineScope result.await()
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
