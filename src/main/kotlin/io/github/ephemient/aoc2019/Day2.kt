package io.github.ephemient.aoc2019

import com.google.common.annotations.VisibleForTesting

class Day2(lines: List<String>) {
    private val ints: List<Int> =
        lines.first().splitToSequence(",").map { it.toInt() }.toList()

    fun part1(): Int = run(noun = 12, verb = 2)

    fun part2(): Int? {
        for (noun in 0..99) {
            for (verb in 0..99) {
                if (run(noun, verb) == 19690720) {
                    return 100 * noun + verb
                }
            }
        }
        return null
    }

    private fun run(noun: Int, verb: Int): Int {
        val mem = ints.toIntArray()
        mem[1] = noun
        mem[2] = verb
        Computer(mem).run()
        return mem[0]
    }

    @VisibleForTesting
    class Computer(@get:VisibleForTesting val mem: IntArray) {
        @VisibleForTesting
        fun step(ip: Int): Int? = when (mem[ip]) {
            1 -> {
                mem[mem[ip + 3]] = mem[mem[ip + 1]] + mem[mem[ip + 2]]
                ip + 4
            }
            2 -> {
                mem[mem[ip + 3]] = mem[mem[ip + 1]] * mem[mem[ip + 2]]
                ip + 4
            }
            99 -> null
            else -> error("unhandled opcode")
        }

        @VisibleForTesting
        fun run() {
            var ip: Int? = 0
            while (ip != null) {
                ip = step(ip)
            }
        }
    }
}
