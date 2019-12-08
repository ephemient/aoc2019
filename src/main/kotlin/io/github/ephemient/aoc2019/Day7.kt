package io.github.ephemient.aoc2019

import com.google.common.annotations.VisibleForTesting
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking

class Day7(private val lines: List<String>) {
    private val ints: List<Int> =
        lines.first().splitToSequence(",").map { it.toInt() }.toList()

    fun part1(): Int? = runBlocking {
        listOf(0, 1, 2, 3, 4)
            .permutations()
            .mapNotNull { amplify(ints, it) }
            .max()
    }

    fun part2(): Int? = runBlocking {
        listOf(5, 6, 7, 8, 9)
            .permutations()
            .mapNotNull { amplify(ints, it) }
            .max()
    }

    companion object {
        @VisibleForTesting
        suspend fun CoroutineScope.amplify(mem: Collection<Int>, order: List<Int>): Int? =
            coroutineScope {
                val channels = order.map { Channel<Int>(Channel.UNLIMITED).apply { send(it) } }
                channels.first().send(0)
                channels.zipWithNext() { input, output ->
                    launch { Intcode(mem.toIntArray()).runAsync(input, output) }
                }
                Intcode(mem.toIntArray()).runAsync(channels.last(), channels.first())
            }

        fun <T> List<T>.permutations(): Iterable<List<T>> = sequence<List<T>> {
            val perm = indices.toMutableList()
            while (true) {
                yield(perm.map(::get))
                val pivot = (0 until lastIndex).lastOrNull { perm[it] < perm[it + 1] } ?: break
                val swap = (pivot + 1 until size).last { perm[it] > perm[pivot] }
                perm[pivot] = perm[swap].also { perm[swap] = perm[pivot] }
                perm.subList(pivot + 1, size).reverse()
            }
        }.asIterable()
    }
}
