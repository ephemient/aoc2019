package io.github.ephemient.aoc2019

import kotlin.math.abs
import kotlin.math.sign

class Day12(lines: List<String>) {
    private val points = lines.map { line ->
        val (x, y, z) = REGEX.findAll(line).take(3).map { it.value.toInt() }.toList()
        Vector(x, y, z)
    }

    private fun simulate(): Sequence<List<Pair<Vector, Vector>>> = sequence {
        val points = points.toMutableList()
        val velocities = MutableList(points.size) { Vector(0, 0, 0) }
        while (true) {
            for ((i, p1) in points.withIndex()) {
                velocities[i] = points.fold(velocities[i]) { acc, p2 -> acc + p1.signTo(p2) }
            }
            for ((i, velocity) in velocities.withIndex()) {
                points[i] = points[i] + velocity
            }
            yield(points.zip(velocities))
        }
    }

    fun part1(n: Int = 1000): Int = simulate().drop(n - 1).first().sumBy { (p, v) ->
        p.absSum() * v.absSum()
    }

    fun part2(): Long {
        val seenX = mutableSetOf<List<Pair<Int, Int>>>()
        val seenY = mutableSetOf<List<Pair<Int, Int>>>()
        val seenZ = mutableSetOf<List<Pair<Int, Int>>>()
        for (state in simulate()) {
            if (!seenX.add(state.map { (p, v) -> p.x to v.x }) &&
                !seenY.add(state.map { (p, v) -> p.y to v.y }) &&
                !seenZ.add(state.map { (p, v) -> p.z to v.z })
            ) {
                break
            }
        }
        return lcm(seenX.size.toLong(), lcm(seenY.size.toLong(), seenZ.size.toLong()))
    }

    private data class Vector(val x: Int, val y: Int, val z: Int) {
        operator fun plus(that: Vector) = Vector(x + that.x, y + that.y, z + that.z)

        fun signTo(that: Vector) = Vector((that.x - x).sign, (that.y - y).sign, (that.z - z).sign)

        fun absSum(): Int = abs(x) + abs(y) + abs(z)
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
    }
}
