package io.github.ephemient.aoc2019

class Day6(private val lines: List<String>) {
    fun part1(): Int {
        val orbits = lines.groupBy(
            keySelector = { it.substringBefore(')') },
            valueTransform = { it.substringAfter(')') }
        )
        val checksums = mutableMapOf<String, Int>()
        fun checksum(name: String): Int = orbits[name]?.sumBy {
            checksum(it) + 1
        } ?: 0
        return orbits.keys.union(orbits.values.flatten()).sumBy { checksum(it) }
    }

    fun part2(): Int {
        val rorbits = lines.associateBy(
            keySelector = { it.substringAfter(')') },
            valueTransform = { it.substringBefore(')') }
        )
        val san = generateSequence("SAN", rorbits::get).toList().asReversed()
        val you = generateSequence("YOU", rorbits::get).toList().asReversed()
        val minLength = minOf(san.count(), you.count())
        val commonPrefix = (0 until minLength)
            .firstOrNull { san[it] != you[it] } ?: minLength
        return san.count() + you.count() - 2 * (commonPrefix + 1)
    }
}
