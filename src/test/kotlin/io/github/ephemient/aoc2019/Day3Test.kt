package io.github.ephemient.aoc2019

import kotlin.test.Test
import kotlin.test.assertEquals

private val SAMPLE_1 =
    """
    R8,U5,L5,D3
    U7,R6,D4,L4
    """.trimIndent().lines()

private val SAMPLE_2 =
    """
    R75,D30,R83,U83,L12,D49,R71,U7,L72
    U62,R66,U55,R34,D71,R55,D58,R83
    """.trimIndent().lines()

private val SAMPLE_3 =
    """
    R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
    U98,R91,D20,R16,D67,R40,U7,R15,U6,R7
    """.trimIndent().lines()

class Day3Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(6, Day3(SAMPLE_1).part1())
        assertEquals(159, Day3(SAMPLE_2).part1())
        assertEquals(135, Day3(SAMPLE_3).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(30, Day3(SAMPLE_1).part2())
        assertEquals(610, Day3(SAMPLE_2).part2())
        assertEquals(410, Day3(SAMPLE_3).part2())
    }
}
