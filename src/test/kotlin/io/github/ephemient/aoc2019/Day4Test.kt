package io.github.ephemient.aoc2019

import kotlin.test.Test
import kotlin.test.assertEquals

class Day4Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(1, Day4(listOf("111111-111111")).part1())
        assertEquals(0, Day4(listOf("223450-223450")).part1())
        assertEquals(0, Day4(listOf("123789-123789")).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(1, Day4(listOf("112233-112233")).part2())
        assertEquals(0, Day4(listOf("123444-123444")).part2())
        assertEquals(1, Day4(listOf("111122-111122")).part2())
    }
}
