package io.github.ephemient.aoc2019

import kotlin.test.Test
import kotlin.test.assertEquals

class Day6Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(
            42,
            Day6(
                listOf(
                    "COM)B",
                    "B)C",
                    "C)D",
                    "D)E",
                    "E)F",
                    "B)G",
                    "G)H",
                    "D)I",
                    "E)J",
                    "J)K",
                    "K)L"
                )
            ).part1()
        )
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(
            4,
            Day6(
                listOf(
                    "COM)B",
                    "B)C",
                    "C)D",
                    "D)E",
                    "E)F",
                    "B)G",
                    "G)H",
                    "D)I",
                    "E)J",
                    "J)K",
                    "K)L",
                    "K)YOU",
                    "I)SAN"
                )
            ).part2()
        )
    }
}
