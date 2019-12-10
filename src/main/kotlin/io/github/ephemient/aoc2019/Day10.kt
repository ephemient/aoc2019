package io.github.ephemient.aoc2019

import kotlin.math.absoluteValue
import kotlin.math.sign

private data class Direction(val sign: Boolean, val numerator: Int, val denominator: Int)

private fun Pair<Int, Int>.directionTo(other: Pair<Int, Int>): Direction {
    val (x, y) = this
    val (u, v) = other
    return if (x == u) {
        Direction(y < v, 1, 0)
    } else {
        val sign = (x - u).sign
        var a = (x - u).absoluteValue
        var b = (y - v).absoluteValue
        while (a != 0) {
            a = (b % a).also { b = a }
        }
        Direction(x > u, (y - v) / b * sign, (x - u) / b * sign)
    }
}

private val directionComparator: Comparator<Direction> = compareBy<Direction> { it.sign }
    .thenBy { it.denominator != 0 }
    .thenComparator { (_, y, x), (_, v, u) -> compareValues(y * u, v * x) }

private fun <T> Iterable<Iterable<T>>.roundRobin(): Sequence<T> = sequence<T> {
    val iterators = mapTo(mutableListOf<Iterator<T>>()) { it.iterator() }
    while (iterators.isNotEmpty()) {
        val iteratorsIterator = iterators.iterator()
        while (iteratorsIterator.hasNext()) {
            val iterator = iteratorsIterator.next()
            if (iterator.hasNext()) {
                yield(iterator.next())
            } else {
                iteratorsIterator.remove()
            }
        }
    }
}

class Day10(lines: List<String>) {
    private val best: Map<Direction, List<Pair<Int, Int>>> = run {
        val field = lines.withIndex().flatMap { (y, line) ->
            line.withIndex().mapNotNull { (x, c) -> if (c == '#') x to y else null }
        }
        val (point, best) = field
            .associateWith { point -> field.filter { it != point }.groupBy(point::directionTo) }
            .entries
            .maxBy { it.value.size } ?: return@run emptyMap()
        val (x, y) = point
        best.mapValues { (_, value) ->
            value.sortedBy { (u, v) -> (x - u).absoluteValue + (y - v).absoluteValue }
        }
    }

    fun part1(): Int = best.size

    fun part2(n: Int = 200): Int? = best.toSortedMap(directionComparator)
        .values
        .roundRobin()
        .drop(n - 1)
        .firstOrNull()
        ?.let { (x, y) -> 100 * x + y }
}
