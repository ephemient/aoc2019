package io.github.ephemient.aoc2019

import kotlin.math.absoluteValue

class Day3(lines: List<String>) {
    private val wires = lines.map { line ->
        line.splitToSequence(",")
            .mapNotNull {
                it[0] to (it.drop(1).toIntOrNull() ?: return@mapNotNull null)
            }
            .toList()
    }

    private inline fun Iterable<Pair<Char, Int>>.walk(crossinline at: (Int, Int) -> Unit) {
        var x = 0
        var y = 0
        for ((c, n) in this) {
            repeat(n) {
                at(x, y)
                when (c) {
                    'U' -> y++
                    'L' -> x--
                    'D' -> y--
                    'R' -> x++
                }
            }
        }
        at(x, y)
    }

    fun part1(): Int? {
        var result: Int? = null
        val field = mutableMapOf<Pair<Int, Int>, Int>()
        for ((i, wire) in wires.withIndex()) {
            wire.walk { x, y ->
                if ((x != 0 || y != 0) && field.getOrPut(x to y) { i } != i) {
                    val manhattan = x.absoluteValue + y.absoluteValue
                    if (result?.let { it > manhattan } != false) {
                        result = manhattan
                    }
                }
            }
        }
        return result
    }

    fun part2(): Int? {
        var result: Int? = null
        var field = mutableMapOf<Pair<Int, Int>, Int>()
        var distances = mutableMapOf<Pair<Int, Int>, Int>()
        for ((i, wire) in wires.withIndex()) {
            var distance = 0
            wire.walk { x, y ->
                when {
                    x == 0 && y == 0 -> {}
                    x to y !in field -> {
                        field[x to y] = i
                        distances[x to y] = distance
                    }
                    field[x to y] != i -> {
                        val other = distances[x to y]
                        if (other != null && result?.let { it > other + distance } != false) {
                            result = other + distance
                        }
                        if (other?.let { it > distance } != false) {
                            field[x to y] = i
                            distances[x to y] = distance
                        }
                    }
                }
                distance++
            }
        }
        return result
    }
}
