package io.github.ephemient.aoc2019

import kotlin.math.abs

class Day24(lines: List<String>) {
    private val start: Set<Pair<Int, Int>> = mutableSetOf<Pair<Int, Int>>().apply {
        for (y in -2..2) {
            for (x in -2..2) {
                if (lines[y + 2][x + 2] == '#') {
                    add(x to y)
                }
            }
        }
    }

    fun part1(): Int {
        var seen = mutableSetOf<Set<Pair<Int, Int>>>()
        var bugs = start
        while (seen.add(bugs)) {
            bugs = bugs.evolve { (x, y) ->
                listOfNotNull(
                    if (x > -2) x - 1 to y else null,
                    if (y > -2) x to y - 1 else null,
                    if (y < 2) x to y + 1 else null,
                    if (x < 2) x + 1 to y else null
                )
            }
        }
        return bugs.sumBy { (x, y) -> 1 shl x + 5 * y + 12 }
    }

    @Suppress("ComplexMethod")
    fun part2(n: Int = 200): Int {
        var bugs: Set<Triple<Int, Int, Int>> = start.mapTo(mutableSetOf()) { (x, y) ->
            Triple(x, y, 0)
        }
        repeat(n) {
            bugs = bugs.evolve { (x, y, z) ->
                val neighbors = mutableListOf<Triple<Int, Int, Int>>()
                if (x > -2 && (x != 1 || y != 0)) {
                    neighbors += Triple(x - 1, y, z)
                }
                if (y > -2 && (x != 0 || y != 1)) {
                    neighbors += Triple(x, y - 1, z)
                }
                if (y < 2 && (x != 0 || y != -1)) {
                    neighbors += Triple(x, y + 1, z)
                }
                if (x < 2 && (x != -1 || y != 0)) {
                    neighbors += Triple(x + 1, y, z)
                }
                if (x == 0 && abs(y) == 1) {
                    (-2..2).mapTo(neighbors) { Triple(it, 2 * y, z - 1) }
                }
                if (y == 0 && abs(x) == 1) {
                    (-2..2).mapTo(neighbors) { Triple(2 * x, it, z - 1) }
                }
                if (abs(x) == 2) {
                    neighbors += Triple(x / 2, 0, z + 1)
                }
                if (abs(y) == 2) {
                    neighbors += Triple(0, y / 2, z + 1)
                }
                neighbors
            }
        }
        return bugs.size
    }

    companion object {
        inline fun <T : Any> Iterable<T>.evolve(neighbors: (T) -> Iterable<T>): Set<T> {
            val counts = mutableMapOf<T, Int>()
            for (x in this) {
                for (y in neighbors(x)) {
                    counts[y] = counts.getOrElse(y) { 0 } + 1
                }
            }
            return counts.entries.mapNotNullTo(mutableSetOf()) { (x, n) ->
                x.takeIf { n == 1 || n == 2 && x !in this }
            }
        }
    }
}
