package io.github.ephemient.aoc2019

class Day1(private val lines: List<String>) {
    private fun fuel(weight: Int): Int? = (weight / 3 - 2).takeIf { it > 0 }

    private fun fuels(weight: Int): Int = generateSequence(fuel(weight), ::fuel).sum()

    fun part1(): Int = lines.sumBy { fuel(it.toInt()) ?: 0 }

    fun part2(): Int = lines.sumBy { fuels(it.toInt()) }
}
