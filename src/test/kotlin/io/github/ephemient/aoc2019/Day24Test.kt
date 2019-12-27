package io.github.ephemient.aoc2019

import kotlin.test.Test
import kotlin.test.assertEquals

class Day24Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(2129920, Day24(SAMPLE).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(99, Day24(SAMPLE).part2(10))
    }

    companion object {
        private val SAMPLE =
            """
            ....#
            #..#.
            #..##
            ..#..
            #....
            """.trimIndent().lines()
    }
}
