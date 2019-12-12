package io.github.ephemient.aoc2019

import kotlin.math.abs
import kotlin.math.sign

class Day12(lines: List<String>) {
    private val numPoints = lines.size

    private val axes: List<List<Int>> = mutableListOf<MutableList<Int>>().apply {
        for (line in lines) {
            for ((i, match) in REGEX.findAll(line).withIndex()) {
                getOrElse(i) { mutableListOf<Int>().also { add(it) } }.add(match.value.toInt())
            }
        }
    }

    fun part1(n: Int = 1000): Int {
        val potentials = MutableList<Int>(numPoints) { 0 }
        val kinetics = MutableList<Int>(numPoints) { 0 }
        for (axis in axes) {
            for ((i, pair) in simulate(axis).drop(n - 1).first().withIndex()) {
                potentials[i] += abs(pair.first)
                kinetics[i] += abs(pair.second)
            }
        }
        return potentials.zip(kinetics) { a, b -> a * b }.sum()
    }

    fun part2(): Long = axes.fold(1L) { acc, axis ->
        lcm(
            acc,
            simulate(axis).indexOfFirst { pairs ->
                pairs.withIndex().all { (i, pair) -> pair.second == 0 && pair.first == axis[i] }
            }.toLong() + 1
        )
    }

    companion object {
        private val REGEX =
            """-?\d+""".toRegex()

        private fun lcm(x: Long, y: Long): Long {
            var a = x
            var b = y
            while (a != 0L) {
                a = (b % a).also { b = a }
            }
            return x / b * y
        }

        private fun simulate(start: Iterable<Int>): Sequence<List<Pair<Int, Int>>> = sequence {
            val points = start.toMutableList()
            val velocities = MutableList(points.size) { 0 }
            while (true) {
                for ((i, point) in points.withIndex()) {
                    velocities[i] += points.sumBy { (it - point).sign }
                }
                for ((i, velocity) in velocities.withIndex()) {
                    points[i] += velocity
                }
                yield(points.zip(velocities))
            }
        }
    }
}
