package io.github.ephemient.aoc2019

import com.google.common.truth.Truth.assertThat
import java.util.stream.IntStream
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
            intArrayOf(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50)
                .also { assertThat(Intcode(it).runBlocking(emptyList())).isEmpty() }
        )
            .asList()
            .containsExactly(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
            .inOrder()
    }

    @Test
    fun `day 2 part 1 example 2`() {
        assertThat(
            intArrayOf(1, 0, 0, 0, 99)
                .also { assertThat(Intcode(it).runBlocking(emptyList())).isEmpty() }
        )
            .asList()
            .containsExactly(2, 0, 0, 0, 99)
            .inOrder()
    }

    @Test
    fun `day 2 part 1 example 3`() {
        assertThat(
            intArrayOf(2, 3, 0, 3, 99)
                .also { assertThat(Intcode(it).runBlocking(emptyList())).isEmpty() }
        )
            .asList()
            .containsExactly(2, 3, 0, 6, 99)
            .inOrder()
    }

    @Test
    fun `day 2 part 1 example 4`() {
        assertThat(
            intArrayOf(2, 4, 4, 5, 99, 0)
                .also { assertThat(Intcode(it).runBlocking(emptyList())).isEmpty() }
        )
            .asList()
            .containsExactly(2, 4, 4, 5, 99, 9801)
            .inOrder()
    }

    @Test
    fun `day 2 part 1 example 5`() {
        assertThat(
            intArrayOf(1, 1, 1, 4, 99, 5, 6, 0, 99)
                .also { assertThat(Intcode(it).runBlocking(emptyList())).isEmpty() }
        )
            .asList()
            .containsExactly(30, 1, 1, 4, 2, 5, 6, 0, 99)
            .inOrder()
    }

    @Test
    fun `day 5 part 1 example 1`() {
        assertThat(
            intArrayOf(1002, 4, 3, 4, 33)
                .also { assertThat(Intcode(it).runBlocking(emptyList())).isEmpty() }
        )
            .asList()
            .containsExactly(1002, 4, 3, 4, 99)
            .inOrder()
    }

    @Test
    fun `day 5 part 1 example 2`() {
        assertThat(
            intArrayOf(1101, 100, -1, 4, 0)
                .also { assertThat(Intcode(it).runBlocking(emptyList())).isEmpty() }
        )
            .asList()
            .containsExactly(1101, 100, -1, 4, 99)
            .inOrder()
    }

    @ParameterizedTest
    @ArgumentsSource(IntsProvider::class)
    fun `day 5 part 2 example 1`(input: Int) {
        assertThat(
            Intcode(intArrayOf(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8)).runBlocking(listOf(input))
        ).containsExactly(if (input == 8) 1 else 0)
    }

    @ParameterizedTest
    @ArgumentsSource(IntsProvider::class)
    fun `day 5 part 2 example 2`(input: Int) {
        assertThat(
            Intcode(intArrayOf(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8)).runBlocking(listOf(input))
        ).containsExactly(if (input < 8) 1 else 0)
    }

    @ParameterizedTest
    @ArgumentsSource(IntsProvider::class)
    fun `day 5 part 2 example 3`(input: Int) {
        assertThat(
            Intcode(intArrayOf(3, 3, 1108, -1, 8, 3, 4, 3, 99)).runBlocking(listOf(input))
        ).containsExactly(if (input == 8) 1 else 0)
    }

    @ParameterizedTest
    @ArgumentsSource(IntsProvider::class)
    fun `day 5 part 2 example 4`(input: Int) {
        assertThat(
            Intcode(intArrayOf(3, 3, 1107, -1, 8, 3, 4, 3, 99)).runBlocking(listOf(input))
        ).containsExactly(if (input < 8) 1 else 0)
    }

    @ParameterizedTest
    @ArgumentsSource(IntsProvider::class)
    fun `day 5 part 2 example 5`(input: Int) {
        assertThat(
            Intcode(intArrayOf(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9))
                .runBlocking(listOf(input))
        ).containsExactly(if (input == 0) 0 else 1)
    }

    @ParameterizedTest
    @ArgumentsSource(IntsProvider::class)
    fun `day 5 part 2 example 6`(input: Int) {
        assertThat(
            Intcode(intArrayOf(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1))
                .runBlocking(listOf(input))
        ).containsExactly(if (input == 0) 0 else 1)
    }

    @ParameterizedTest
    @ArgumentsSource(IntsProvider::class)
    fun `day 5 part 2 example 7`(input: Int) {
        assertThat(
            Intcode(
                intArrayOf(
                    3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36,
                    98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101,
                    1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99
                )
            ).runBlocking(listOf(input))
        ).containsExactly(
            if (input < 8) 999 else if (input == 8) 1000 else if (input > 8) 1001 else error("??!")
        )
    }

    class IntsProvider : ArgumentsProvider {
        override fun provideArguments(context: ExtensionContext): Stream<Arguments> =
            IntStream.range(-50, 50).mapToObj { Arguments.of(it) }
    }
}
