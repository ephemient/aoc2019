package io.github.ephemient.aoc2019

import kotlin.test.Test
import kotlin.test.assertEquals

class Day1Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(2, Day1(listOf("12")).part1())
        assertEquals(2, Day1(listOf("14")).part1())
        assertEquals(654, Day1(listOf("1969")).part1())
        assertEquals(33583, Day1(listOf("100756")).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(2, Day1(listOf("14")).part2())
        assertEquals(966, Day1(listOf("1969")).part2())
        assertEquals(50346, Day1(listOf("100756")).part2())
    }
}
