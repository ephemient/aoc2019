package io.github.ephemient.aoc2019

import com.google.common.math.IntMath
import kotlinx.coroutines.runBlocking

class Intcode(private val mem: MutableList<Long>) {
    private var base = 0
    private var ip = 0

    private fun arg(n: Int): Int =
        when (val mode = mem[ip].toInt() / IntMath.pow(10, n + 1) % 10) {
            0 -> mem[ip + n].toInt()
            1 -> ip + n
            2 -> base + mem[ip + n].toInt()
            else -> error("bad mode $mode")
        }

    private operator fun get(n: Int): Long = mem.getOrElse(arg(n)) { 0 }

    private operator fun set(n: Int, value: Long) {
        val addr = arg(n)
        if (addr < mem.size) {
            mem[addr] = value
        } else {
            if (mem is ArrayList) {
                mem.ensureCapacity(addr + 1)
            }
            repeat(addr - mem.size) { mem.add(0L) }
            mem.add(value)
        }
    }

    fun runBlocking(input: Iterable<Long>): List<Long> = runBlocking {
        mutableListOf<Long>().apply {
            val inputIterator = input.iterator()
            while (true) {
                add(getOutput({ inputIterator.next() }) ?: break)
            }
        }
    }

    @Suppress("ComplexMethod")
    suspend fun getOutput(input: suspend () -> Long): Long? {
        while (true) {
            when (mem[ip].toInt() % 100) {
                1 -> {
                    this[3] = this[1] + this[2]
                    ip += 4
                }
                2 -> {
                    this[3] = this[1] * this[2]
                    ip += 4
                }
                3 -> {
                    this[1] = input()
                    ip += 2
                }
                4 -> {
                    val output = this[1]
                    ip += 2
                    return output
                }
                5 -> ip = if (this[1] != 0L) this[2].toInt() else ip + 3
                6 -> ip = if (this[1] == 0L) this[2].toInt() else ip + 3
                7 -> {
                    this[3] = if (this[1] < this[2]) 1 else 0
                    ip += 4
                }
                8 -> {
                    this[3] = if (this[1] == this[2]) 1 else 0
                    ip += 4
                }
                9 -> {
                    base += this[1].toInt()
                    ip += 2
                }
                99 -> return null
                else -> error("bad opcode ${mem[ip]}")
            }
        }
    }
}
