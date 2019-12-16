package io.github.ephemient.aoc2019

import kotlinx.coroutines.runBlocking

class Day15(lines: List<String>) {
    private val ints: List<Long> =
        lines.first().splitToSequence(",").map { it.toLong() }.toList()

    private class Bot(private val vm: Intcode) {
        val visited: Set<Pair<Int, Int>>
            get() = _visited
        val spaces: Set<Pair<Int, Int>>
            get() = _spaces
        val oxygen: Pair<Int, Int>?
            get() = _oxygen

        private val _visited = mutableSetOf<Pair<Int, Int>>()
        private val _spaces = mutableSetOf<Pair<Int, Int>>()
        private var _oxygen: Pair<Int, Int>? = null
        private var frontier = mutableSetOf(0 to 0)

        private var x = 0
        private var y = 0

        private var moves = listOf<Triple<Int, Int, Long>>()

        suspend fun explore() {
            while (planMoves()) {
                moves@for ((x, y, direction) in moves) {
                    when (vm.getOutput({ direction })) {
                        0L -> break@moves
                        1L -> {}
                        2L -> _oxygen = x to y
                        else -> error("unexpected output or termination")
                    }
                    this.x = x
                    this.y = y
                }
            }
        }

        private fun planMoves(): Boolean {
            _spaces.add(x to y)
            frontier.addAll(listOf(x to y - 1, x to y + 1, x - 1 to y, x + 1 to y) - _visited)
            val (x, y) = with(frontier.iterator().takeIf { it.hasNext() } ?: return false) {
                next().also { remove() }
            }
            moves = path(this.x to this.y, x to y, _spaces)
            _visited.add(x to y)
            return true
        }
    }

    fun part1(): Int? {
        val bot = Bot(Intcode(ints.toMutableList()))
        runBlocking { bot.explore() }
        return path(0 to 0, bot.oxygen ?: return null, bot.spaces).size
    }

    fun part2(): Int? {
        val bot = Bot(Intcode(ints.toMutableList()))
        runBlocking { bot.explore() }
        val oxygen = bot.oxygen ?: return null
        return bot.spaces.map { path(oxygen, it, bot.spaces).size }.max()
    }

    companion object {
        private fun path(
            src: Pair<Int, Int>,
            dst: Pair<Int, Int>,
            pass: Set<Pair<Int, Int>>
        ): List<Triple<Int, Int, Long>> {
            if (src == dst) {
                return emptyList()
            }
            val seen = mutableSetOf(src)
            val queue = mutableListOf<Pair<Pair<Int, Int>, List<Triple<Int, Int, Long>>>>(
                src to emptyList()
            )
            while (true) {
                val (pos, moves) = queue.removeAt(0)
                val (x, y) = pos
                for (
                    next in
                    listOf(
                        Triple(x, y - 1, 1L), Triple(x, y + 1, 2L),
                        Triple(x - 1, y, 3L), Triple(x + 1, y, 4L)
                    )
                ) {
                    val pos2 = next.first to next.second
                    if (pos2 == dst) {
                        return moves + next
                    }
                    if (pos2 in seen || pos2 !in pass) {
                        continue
                    }
                    seen.add(pos2)
                    queue.add(pos2 to moves + next)
                }
            }
        }
    }
}
