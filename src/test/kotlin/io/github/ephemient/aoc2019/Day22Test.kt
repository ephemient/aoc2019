package io.github.ephemient.aoc2019

import io.github.ephemient.aoc2019.Day22.Shuffle
import kotlin.test.Test
import kotlin.test.assertEquals

class Day22Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(
            listOf(0L, 3L, 6L, 9L, 2L, 5L, 8L, 1L, 4L, 7L),
            Day22(
                """
                deal with increment 7
                deal into new stack
                deal into new stack
                """.trimIndent().lines()
            ).shuffle(10L).flatten()
        )
        assertEquals(
            listOf(3L, 0L, 7L, 4L, 1L, 8L, 5L, 2L, 9L, 6L),
            Day22(
                """
                cut 6
                deal with increment 7
                deal into new stack
                """.trimIndent().lines()
            ).shuffle(10L).flatten()
        )
        assertEquals(
            listOf(6L, 3L, 0L, 7L, 4L, 1L, 8L, 5L, 2L, 9L),
            Day22(
                """
                deal with increment 7
                deal with increment 9
                cut -2
                """.trimIndent().lines()
            ).shuffle(10L).flatten()
        )
        assertEquals(
            listOf(9L, 2L, 5L, 8L, 1L, 4L, 7L, 0L, 3L, 6L),
            Day22(
                """
                deal into new stack
                cut -2
                deal with increment 7
                cut 8
                cut -4
                deal with increment 7
                cut 3
                deal with increment 9
                deal with increment 3
                cut -1
                """.trimIndent().lines()
            ).shuffle(10L).flatten()
        )
    }

    companion object {
        private fun Shuffle.flatten(): List<Long>? {
            val mapping = (0 until base).associateBy(::invoke)
            return (0 until base).map { mapping[it] ?: return null }
        }
    }
}
