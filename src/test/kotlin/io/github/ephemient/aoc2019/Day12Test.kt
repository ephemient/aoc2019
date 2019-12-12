package io.github.ephemient.aoc2019

import kotlin.test.Test
import kotlin.test.assertEquals

class Day12Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(179, Day12(SAMPLE_1).part1(10))
        assertEquals(1940, Day12(SAMPLE_2).part1(100))
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(2772L, Day12(SAMPLE_1).part2())
        assertEquals(4686774924L, Day12(SAMPLE_2).part2())
    }

    companion object {
        private val SAMPLE_1 =
            """
            <x=-1, y=0, z=2>
            <x=2, y=-10, z=-7>
            <x=4, y=-8, z=8>
            <x=3, y=5, z=-1>
            """.trimIndent().lines()

        private val SAMPLE_2 =
            """
            <x=-8, y=-10, z=0>
            <x=5, y=5, z=10>
            <x=2, y=-7, z=3>
            <x=9, y=-8, z=-3>
            """.trimIndent().lines()
    }
}
