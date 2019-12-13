package io.github.ephemient.aoc2019

import com.google.common.annotations.VisibleForTesting
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking

class Day7(lines: List<String>) {
    private val ints: List<Long> =
        lines.first().splitToSequence(",").map { it.toLong() }.toList()

    fun part1(): Long? = runBlocking {
        listOf(0, 1, 2, 3, 4)
            .permutations()
            .mapNotNull { amplify(ints, it) }
            .max()
    }

    fun part2(): Long? = runBlocking {
        listOf(5, 6, 7, 8, 9)
            .permutations()
            .mapNotNull { amplify(ints, it) }
            .max()
    }

    companion object {
        @VisibleForTesting
        suspend fun CoroutineScope.amplify(mem: Collection<Long>, order: List<Int>): Long? =
            coroutineScope {
                val channels = order.map {
                    Channel<Long>(Channel.UNLIMITED).apply { send(it.toLong()) }
                }
                channels.first().send(0)
                channels.zipWithNext() { input, output ->
                    launch { Intcode(mem.toMutableList()).runAsync(input::receive, output::send) }
                }
                Intcode(mem.toMutableList())
                    .runAsync(channels.last()::receive, channels.first()::send)
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
