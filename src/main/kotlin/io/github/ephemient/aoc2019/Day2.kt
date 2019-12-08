package io.github.ephemient.aoc2019

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
        Intcode(mem).runBlocking(emptyList())
        return mem[0]
    }
}
