package io.github.ephemient.aoc2019

import com.google.common.annotations.VisibleForTesting

class Day22(private val lines: List<String>) {
    @VisibleForTesting
    fun shuffle(base: Long): Shuffle = lines.fold(Shuffle(base)) { acc, line ->
        RE.matchEntire(line)?.groups?.let {
            it[1]?.value?.let { acc + -it.toLong() }
                ?: it[2]?.value?.let { acc * it.toLong() }
                ?: acc.reverse()
        } ?: acc
    }

    fun part1(): Long = shuffle(10007L)(2019L)

    fun part2(): Long = shuffle(119315717514047L).pow(101741582076661L)[2020L]

    @VisibleForTesting
    data class Shuffle(val base: Long, val m: Long = 1, val c: Long = 0) {
        init {
            check(base > 0L)
            check(m in 0L until base)
            check(c in 0L until base)
        }

        fun reverse(): Shuffle = copy(m = properMod(-m, base), c = properMod(-1 - c, base))

        operator fun plus(n: Long): Shuffle = copy(c = properMod(c + n, base))

        operator fun times(n: Long): Shuffle = copy(
            m = timesMod(m, n, base),
            c = timesMod(c, n, base)
        )

        operator fun times(other: Shuffle): Shuffle {
            check(base == other.base)
            return copy(
                m = timesMod(m, other.m, base),
                c = properMod(timesMod(c, other.m, base) + other.c, base)
            )
        }

        fun pow(x: Long): Shuffle {
            check(x >= 0L)
            return when {
                x == 0L -> Shuffle(base)
                x % 2L == 0L -> (this * this).pow(x / 2L)
                else -> this * pow(x - 1L)
            }
        }

        operator fun invoke(x: Long): Long = properMod(timesMod(m, x, base) + c, base)

        operator fun get(x: Long): Long {
            check(base.toBigInteger().isProbablePrime(10))
            return pow(base - 2)(x)
        }
    }

    companion object {
        private val RE =
            """
            deal \s+ into \s+ new \s+ stack |
            cut \s+ (-?[1-9][0-9]*) |
            deal \s+ with \s+ increment \s+ ([0-9]+)
            """.toRegex(RegexOption.COMMENTS)
    }
}
