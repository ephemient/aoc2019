package io.github.ephemient.aoc2019

import kotlin.math.abs

class Day16(lines: List<String>) {
    private val input: List<Int> = lines.first().map { Character.digit(it, 10) }

    fun part1(times: Int = 100): String {
        var value = input
        repeat(times) {
            value = value.indices.map { i ->
                var sum = 0
                for ((j, v) in value.withIndex()) {
                    when ((j + 1) % (4 * i + 4) / (i + 1)) {
                        1 -> sum += v
                        3 -> sum -= v
                    }
                }
                abs(sum) % 10
            }
        }
        return value.take(8).joinToString("")
    }

    fun part2(times: Int = 100): String {
        val offset = input.take(7).fold(0) { acc, digit -> 10 * acc + digit }
        val length = input.size
        val newLength = 10000 * length
        check(offset < newLength && newLength <= 2 * offset)
        val value = (offset until newLength).map { input[it % length] }.toIntArray()
        repeat(times) {
            for (i in value.lastIndex downTo 1) {
                value[i - 1] += value[i]
            }
            for (i in value.indices) {
                value[i] = abs(value[i]) % 10
            }
        }
        return value.take(8).joinToString("")
    }
}
