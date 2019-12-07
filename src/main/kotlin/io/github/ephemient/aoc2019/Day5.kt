package io.github.ephemient.aoc2019

import com.google.common.annotations.VisibleForTesting
import com.google.common.math.IntMath

class Day5(lines: List<String>) {
    private val ints: List<Int> =
        lines.first().splitToSequence(",").map { it.toInt() }.toList()

    fun part1(): Int? = mutableListOf<Int>().apply {
        Computer(ints.toIntArray()).run({ 1 }, { add(it) })
    }.lastOrNull()

    fun part2(): Int? = mutableListOf<Int>().apply {
        Computer(ints.toIntArray()).run({ 5 }, { add(it) })
    }.lastOrNull()

    class Computer(private val mem: IntArray) {
        private operator fun IntArray.get(ip: Int, n: Int): Int =
            if (this[ip] / IntMath.pow(10, n + 1) % 10 == 0) {
                mem[mem[ip + n]]
            } else {
                mem[ip + n]
            }

        private operator fun IntArray.set(ip: Int, n: Int, value: Int) {
            if (this[ip] / IntMath.pow(10, n + 1) % 10 == 0) {
                mem[mem[ip + n]] = value
            } else {
                mem[ip + n] = value
            }
        }

        @Suppress("ComplexMethod")
        @VisibleForTesting
        fun step(ip: Int, input: () -> Int, output: (Int) -> Unit): Int? =
            when (mem[ip] % 100) {
                1 -> {
                    mem[ip, 3] = mem[ip, 1] + mem[ip, 2]
                    ip + 4
                }
                2 -> {
                    mem[ip, 3] = mem[ip, 1] * mem[ip, 2]
                    ip + 4
                }
                3 -> {
                    mem[ip, 1] = input()
                    ip + 2
                }
                4 -> {
                    output(mem[ip, 1])
                    ip + 2
                }
                5 -> if (mem[ip, 1] != 0) mem[ip, 2] else ip + 3
                6 -> if (mem[ip, 1] == 0) mem[ip, 2] else ip + 3
                7 -> {
                    mem[ip, 3] = if (mem[ip, 1] < mem[ip, 2]) 1 else 0
                    ip + 4
                }
                8 -> {
                    mem[ip, 3] = if (mem[ip, 1] == mem[ip, 2]) 1 else 0
                    ip + 4
                }
                99 -> null
                else -> error("unhandled opcode ${mem[ip]}")
            }

        fun run(input: () -> Int, output: (Int) -> Unit) {
            var ip: Int? = 0
            while (ip != null) {
                ip = step(ip, input, output)
            }
        }
    }
}
