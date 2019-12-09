package io.github.ephemient.aoc2019

class Day2(lines: List<String>) {
    private val ints: List<Long> =
        lines.first().splitToSequence(",").map { it.toLong() }.toList()

    fun part1(): Long = run(noun = 12, verb = 2)

    fun part2(): Long? {
        for (noun in 0L..99) {
            for (verb in 0L..99) {
                if (run(noun, verb) == 19690720L) {
                    return 100 * noun + verb
                }
            }
        }
        return null
    }

    private fun run(noun: Long, verb: Long): Long {
        val mem = ints.toMutableList()
        mem[1] = noun
        mem[2] = verb
        Intcode(mem).runBlocking(emptyList())
        return mem[0]
    }
}
