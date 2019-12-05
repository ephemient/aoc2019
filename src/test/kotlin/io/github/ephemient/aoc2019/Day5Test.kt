package io.github.ephemient.aoc2019

import com.google.common.truth.Truth.assertThat
import java.util.Collections.emptyIterator
import java.util.Collections.emptyList
import java.util.stream.IntStream
import java.util.stream.Stream
import kotlin.test.Test
import org.junit.jupiter.api.extension.ExtensionContext
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.ArgumentsProvider
import org.junit.jupiter.params.provider.ArgumentsSource

class Day5Test {
    @Test
    fun `part 1 example 1`() {
        val mem = intArrayOf(1002, 4, 3, 4, 33)
        val computer = Day5.Computer(mem)
        assertThat(computer.step(0, emptyIterator(), emptyList())).isEqualTo(4)
        assertThat(mem).asList()
            .containsExactly(1002, 4, 3, 4, 99)
            .inOrder()
        assertThat(computer.step(4, emptyIterator(), emptyList())).isNull()
        assertThat(mem).asList()
            .containsExactly(1002, 4, 3, 4, 99)
            .inOrder()
    }

    @Test
    fun `part 1 example 2`() {
        val mem = intArrayOf(1101, 100, -1, 4, 0)
        val computer = Day5.Computer(mem)
        assertThat(computer.step(0, emptyIterator(), emptyList())).isEqualTo(4)
        assertThat(mem).asList()
            .containsExactly(1101, 100, -1, 4, 99)
            .inOrder()
        assertThat(computer.step(4, emptyIterator(), emptyList())).isNull()
        assertThat(mem).asList()
            .containsExactly(1101, 100, -1, 4, 99)
            .inOrder()
    }

    @ParameterizedTest
    @ArgumentsSource(IntsProvider::class)
    fun `part 2 example 1`(input: Int) {
        assertThat(
            Day5.Computer(intArrayOf(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8))
                .run(listOf(input))
                .lastOrNull()
        ).isEqualTo(if (input == 8) 1 else 0)
    }

    @ParameterizedTest
    @ArgumentsSource(IntsProvider::class)
    fun `part 2 example 2`(input: Int) {
        assertThat(
            Day5.Computer(intArrayOf(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8))
                .run(listOf(input))
                .lastOrNull()
        ).isEqualTo(if (input < 8) 1 else 0)
    }

    @ParameterizedTest
    @ArgumentsSource(IntsProvider::class)
    fun `part 2 example 3`(input: Int) {
        assertThat(
            Day5.Computer(intArrayOf(3, 3, 1108, -1, 8, 3, 4, 3, 99))
                .run(listOf(input))
                .lastOrNull()
        ).isEqualTo(if (input == 8) 1 else 0)
    }

    @ParameterizedTest
    @ArgumentsSource(IntsProvider::class)
    fun `part 2 example 4`(input: Int) {
        assertThat(
            Day5.Computer(intArrayOf(3, 3, 1107, -1, 8, 3, 4, 3, 99))
                .run(listOf(input))
                .lastOrNull()
        ).isEqualTo(if (input < 8) 1 else 0)
    }

    @ParameterizedTest
    @ArgumentsSource(IntsProvider::class)
    fun `part 2 example 5`(input: Int) {
        assertThat(
            Day5.Computer(intArrayOf(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9))
                .run(listOf(input))
                .lastOrNull()
        ).isEqualTo(if (input == 0) 0 else 1)
    }

    @ParameterizedTest
    @ArgumentsSource(IntsProvider::class)
    fun `part 2 example 6`(input: Int) {
        assertThat(
            Day5.Computer(intArrayOf(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1))
                .run(listOf(input))
                .lastOrNull()
        ).isEqualTo(if (input == 0) 0 else 1)
    }

    @ParameterizedTest
    @ArgumentsSource(IntsProvider::class)
    fun `part 2 example 7`(input: Int) {
        assertThat(
            Day5.Computer(
                intArrayOf(
                    3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36,
                    98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101,
                    1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99
                )
            )
                .run(listOf(input))
                .lastOrNull()
        ).isEqualTo(
            if (input < 8) 999 else if (input == 8) 1000 else if (input > 8) 1001 else error("??!")
        )
    }

    class IntsProvider : ArgumentsProvider {
        override fun provideArguments(context: ExtensionContext): Stream<Arguments> =
            IntStream.range(-50, 50).mapToObj { Arguments.of(it) }
    }
}
