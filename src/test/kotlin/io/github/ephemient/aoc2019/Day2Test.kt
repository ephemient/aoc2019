package io.github.ephemient.aoc2019

import com.google.common.truth.Truth.assertThat
import kotlin.test.Test

class Day2Test {
    @Test
    fun `part 1 example 1`() {
        val mem = intArrayOf(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50)
        val computer = Day2.Computer(mem)
        assertThat(computer.step(0)).isEqualTo(4)
        assertThat(mem).asList()
            .containsExactly(1, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
            .inOrder()
        assertThat(computer.step(4)).isEqualTo(8)
        assertThat(mem).asList()
            .containsExactly(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
            .inOrder()
        assertThat(computer.step(8)).isNull()
        assertThat(mem).asList()
            .containsExactly(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
            .inOrder()
    }

    @Test
    fun `part 1 example 2`() {
        assertThat(Day2.Computer(intArrayOf(1, 0, 0, 0, 99)).apply { run() }.mem)
            .asList()
            .containsExactly(2, 0, 0, 0, 99)
            .inOrder()
    }

    @Test
    fun `part 1 example 3`() {
        assertThat(Day2.Computer(intArrayOf(2, 3, 0, 3, 99)).apply { run() }.mem)
            .asList()
            .containsExactly(2, 3, 0, 6, 99)
            .inOrder()
    }

    @Test
    fun `part 1 example 4`() {
        assertThat(Day2.Computer(intArrayOf(2, 4, 4, 5, 99, 0)).apply { run() }.mem)
            .asList()
            .containsExactly(2, 4, 4, 5, 99, 9801)
            .inOrder()
    }

    @Test
    fun `part 1 example 5`() {
        assertThat(Day2.Computer(intArrayOf(1, 1, 1, 4, 99, 5, 6, 0, 99)).apply { run() }.mem)
            .asList()
            .containsExactly(30, 1, 1, 4, 2, 5, 6, 0, 99)
            .inOrder()
    }
}
