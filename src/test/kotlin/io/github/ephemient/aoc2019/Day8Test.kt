package io.github.ephemient.aoc2019

import kotlin.test.Test
import kotlin.test.assertEquals

class Day8Test {
    @Test
    fun `part 2 examples`() {
        assertEquals("\u2592\u2593\n\u2593\u2592", Day8(listOf("0222112222120000"), 2, 2).part2())
    }
}
