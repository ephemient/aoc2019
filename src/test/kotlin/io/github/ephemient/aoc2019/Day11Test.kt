package io.github.ephemient.aoc2019

import kotlin.test.Test
import kotlin.test.assertEquals

private const val EXAMPLE_BOT = "3,0,5,0,-1,104,1,104,0,3,0,5,0,-1,104,0,104,0,3,0,104,1,104,0,3,0,104,1,104,0,3,0,6,0,-1,104,0,104,1,3,0,104,1,104,0,3,0,104,1,104,0,99"

class Day11Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(6, Day11(listOf(EXAMPLE_BOT)).part1())
    }
}
