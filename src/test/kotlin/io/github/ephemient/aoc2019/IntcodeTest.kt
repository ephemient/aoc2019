package io.github.ephemient.aoc2019

import com.google.common.truth.Truth.assertThat
import java.util.stream.LongStream
import java.util.stream.Stream
import kotlin.test.Test
import org.junit.jupiter.api.extension.ExtensionContext
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.ArgumentsProvider
import org.junit.jupiter.params.provider.ArgumentsSource

class IntcodeTest {
    @Test
    fun `day 2 part 1 example 1`() {
        assertThat(
            mutableListOf<Long>(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50)
                .also { assertThat(Intcode(it).runBlocking(emptyList())).isEmpty() }
        )
            .containsExactly(3500L, 9L, 10L, 70L, 2L, 3L, 11L, 0L, 99L, 30L, 40L, 50L)
            .inOrder()
    }

    @Test
    fun `day 2 part 1 example 2`() {
        assertThat(
            mutableListOf<Long>(1, 0, 0, 0, 99)
                .also { assertThat(Intcode(it).runBlocking(emptyList())).isEmpty() }
        )
            .containsExactly(2L, 0L, 0L, 0L, 99L)
            .inOrder()
    }

    @Test
    fun `day 2 part 1 example 3`() {
        assertThat(
            mutableListOf<Long>(2, 3, 0, 3, 99)
                .also { assertThat(Intcode(it).runBlocking(emptyList())).isEmpty() }
        )
            .containsExactly(2L, 3L, 0L, 6L, 99L)
            .inOrder()
    }

    @Test
    fun `day 2 part 1 example 4`() {
        assertThat(
            mutableListOf<Long>(2, 4, 4, 5, 99, 0)
                .also { assertThat(Intcode(it).runBlocking(emptyList())).isEmpty() }
        )
            .containsExactly(2L, 4L, 4L, 5L, 99L, 9801L)
            .inOrder()
    }

    @Test
    fun `day 2 part 1 example 5`() {
        assertThat(
            mutableListOf<Long>(1, 1, 1, 4, 99, 5, 6, 0, 99)
                .also { assertThat(Intcode(it).runBlocking(emptyList())).isEmpty() }
        )
            .containsExactly(30L, 1L, 1L, 4L, 2L, 5L, 6L, 0L, 99L)
            .inOrder()
    }

    @Test
    fun `day 5 part 1 example 1`() {
        assertThat(
            mutableListOf<Long>(1002, 4, 3, 4, 33)
                .also { assertThat(Intcode(it).runBlocking(emptyList())).isEmpty() }
        )
            .containsExactly(1002L, 4L, 3L, 4L, 99L)
            .inOrder()
    }

    @Test
    fun `day 5 part 1 example 2`() {
        assertThat(
            mutableListOf<Long>(1101, 100, -1, 4, 0)
                .also { assertThat(Intcode(it).runBlocking(emptyList())).isEmpty() }
        )
            .containsExactly(1101L, 100L, -1L, 4L, 99L)
            .inOrder()
    }

    @ParameterizedTest
    @ArgumentsSource(LongsProvider::class)
    fun `day 5 part 2 example 1`(input: Long) {
        assertThat(
            Intcode(mutableListOf(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8)).runBlocking(listOf(input))
        ).containsExactly(if (input == 8L) 1L else 0L)
    }

    @ParameterizedTest
    @ArgumentsSource(LongsProvider::class)
    fun `day 5 part 2 example 2`(input: Long) {
        assertThat(
            Intcode(mutableListOf(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8)).runBlocking(listOf(input))
        ).containsExactly(if (input < 8L) 1L else 0L)
    }

    @ParameterizedTest
    @ArgumentsSource(LongsProvider::class)
    fun `day 5 part 2 example 3`(input: Long) {
        assertThat(
            Intcode(mutableListOf(3, 3, 1108, -1, 8, 3, 4, 3, 99)).runBlocking(listOf(input))
        ).containsExactly(if (input == 8L) 1L else 0L)
    }

    @ParameterizedTest
    @ArgumentsSource(LongsProvider::class)
    fun `day 5 part 2 example 4`(input: Long) {
        assertThat(
            Intcode(mutableListOf(3, 3, 1107, -1, 8, 3, 4, 3, 99)).runBlocking(listOf(input))
        ).containsExactly(if (input < 8L) 1L else 0L)
    }

    @ParameterizedTest
    @ArgumentsSource(LongsProvider::class)
    fun `day 5 part 2 example 5`(input: Long) {
        assertThat(
            Intcode(mutableListOf(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9))
                .runBlocking(listOf(input))
        ).containsExactly(if (input == 0L) 0L else 1L)
    }

    @ParameterizedTest
    @ArgumentsSource(LongsProvider::class)
    fun `day 5 part 2 example 6`(input: Long) {
        assertThat(
            Intcode(mutableListOf(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1))
                .runBlocking(listOf(input))
        ).containsExactly(if (input == 0L) 0L else 1L)
    }

    @ParameterizedTest
    @ArgumentsSource(LongsProvider::class)
    fun `day 5 part 2 example 7`(input: Long) {
        assertThat(
            Intcode(
                mutableListOf(
                    3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36,
                    98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101,
                    1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99
                )
            ).runBlocking(listOf(input))
        ).containsExactly(
            if (input < 8L) {
                999L
            } else if (input == 8L) {
                1000L
            } else if (input > 8L) {
                1001L
            } else {
                error("??!")
            }
        )
    }

    @Test
    fun `day 9 part 1 example 1`() {
        val quine = listOf<Long>(
            109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99
        )
        assertThat(Intcode(quine.toMutableList()).runBlocking(emptyList())).isEqualTo(quine)
    }

    @Test
    fun `day 9 part 1 example 2`() {
        assertThat(
            Intcode(mutableListOf(1102, 34915192, 34915192, 7, 4, 7, 99, 0))
                .runBlocking(emptyList())
        ).containsExactly(1219070632396864L)
    }

    @Test
    fun `day 9 part 1 example 3`() {
        assertThat(
            Intcode(mutableListOf(104, 1125899906842624, 99)).runBlocking(emptyList())
        ).containsExactly(1125899906842624L)
    }

    class LongsProvider : ArgumentsProvider {
        override fun provideArguments(context: ExtensionContext): Stream<Arguments> =
            LongStream.range(-50, 50).mapToObj { Arguments.of(it) }
    }
}
