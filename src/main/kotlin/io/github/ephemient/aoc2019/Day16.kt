package io.github.ephemient.aoc2019

import kotlin.math.abs

class Day16(lines: List<String>) {
    private val input: List<Int> = lines.first().map { Character.digit(it, 10) }

    fun part1(times: Int = 100): String {
        val value = input.toIntArray()
        repeat(times) {
            for (x in value.indices) {
                var sign = true
                value[x] = abs(
                    (x until value.size step 2 * x + 2).sumBy { base ->
                        (base until minOf(base + x + 1, value.size)).sumBy(value::get)
                            .let { if (sign) it else -it }
                            .also { sign = !sign }
                    }
                ) % 10
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
            value.indices.reversed().fold(0) { acc, i ->
                (abs(acc + value[i]) % 10).also { value[i] = it }
            }
        }
        return value.take(8).joinToString("")
    }
}
