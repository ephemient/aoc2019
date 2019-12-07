package io.github.ephemient.aoc2019

import com.google.common.annotations.VisibleForTesting
import io.github.ephemient.aoc2019.Day5.Computer
import java.util.concurrent.LinkedBlockingQueue
import kotlin.concurrent.thread

class Day7(private val lines: List<String>) {
    private val ints: List<Int> =
        lines.first().splitToSequence(",").map { it.toInt() }.toList()

    fun part1(): Int? = listOf(0, 1, 2, 3, 4)
        .permutations()
        .mapNotNull { amplify(ints, it) }
        .max()

    fun part2(): Int? = listOf(5, 6, 7, 8, 9)
        .permutations()
        .mapNotNull { amplify(ints, it) }
        .max()

    companion object {
        @VisibleForTesting
        fun amplify(mem: Collection<Int>, inputs: List<Int>): Int? {
            val channels = inputs.map {
                LinkedBlockingQueue<Int>().apply { add(it) }
            }.apply { first().add(0) }
            for (i in 0 until inputs.lastIndex) {
                thread {
                    Computer(mem.toIntArray()).run(
                        input = channels[i]::take,
                        output = channels[i + 1]::put
                    )
                }
            }
            var lastOutput: Int? = null
            Computer(mem.toIntArray()).run(
                input = channels.last()::take,
                output = with(channels.first()) {
                    { value: Int -> put(value.also { lastOutput = it }) }
                }
            )
            return lastOutput
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
