package io.github.ephemient.aoc2019

import com.google.common.math.IntMath
import kotlinx.coroutines.async
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.channels.ReceiveChannel
import kotlinx.coroutines.channels.SendChannel
import kotlinx.coroutines.channels.toList
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking

class Intcode(private val mem: MutableList<Long>) {
    private fun arg(ip: Int, base: Int, n: Int): Int =
        when (val mode = mem[ip].toInt() / IntMath.pow(10, n + 1) % 10) {
            0 -> mem[ip + n].toInt()
            1 -> ip + n
            2 -> base + mem[ip + n].toInt()
            else -> error("bad mode $mode")
        }

    private operator fun List<Long>.get(ip: Int, base: Int, n: Int): Long =
        mem.getOrElse(arg(ip, base, n)) { 0 }

    private operator fun MutableList<Long>.set(ip: Int, base: Int, n: Int, value: Long) {
        val addr = arg(ip, base, n)
        if (addr < size) {
            mem[arg(ip, base, n)] = value
        } else {
            if (this is ArrayList) {
                ensureCapacity(addr + 1)
            }
            addAll(List(addr - size) { 0L })
            add(value)
        }
    }

    fun runBlocking(input: Iterable<Long>): List<Long> = runBlocking {
        val inputChannel = Channel<Long>()
        val outputChannel = Channel<Long>()
        launch {
            for (value in input) {
                inputChannel.send(value)
            }
        }
        val deferredOutput = async { outputChannel.toList() }
        runAsync(inputChannel, outputChannel)
        outputChannel.close()
        deferredOutput.await()
    }

    @Suppress("ComplexMethod")
    suspend fun runAsync(input: ReceiveChannel<Long>, output: SendChannel<Long>): Long? {
        var ip = 0
        var base = 0
        var lastOutput: Long? = null
        while (true) {
            when (mem[ip].toInt() % 100) {
                1 -> {
                    mem[ip, base, 3] = mem[ip, base, 1] + mem[ip, base, 2]
                    ip += 4
                }
                2 -> {
                    mem[ip, base, 3] = mem[ip, base, 1] * mem[ip, base, 2]
                    ip += 4
                }
                3 -> {
                    mem[ip, base, 1] = input.receive()
                    ip += 2
                }
                4 -> {
                    output.send(mem[ip, base, 1].also { lastOutput = it })
                    ip += 2
                }
                5 -> ip = if (mem[ip, base, 1] != 0L) mem[ip, base, 2].toInt() else ip + 3
                6 -> ip = if (mem[ip, base, 1] == 0L) mem[ip, base, 2].toInt() else ip + 3
                7 -> {
                    mem[ip, base, 3] = if (mem[ip, base, 1] < mem[ip, base, 2]) 1 else 0
                    ip += 4
                }
                8 -> {
                    mem[ip, base, 3] = if (mem[ip, base, 1] == mem[ip, base, 2]) 1 else 0
                    ip += 4
                }
                9 -> {
                    base += mem[ip, base, 1].toInt()
                    ip += 2
                }
                99 -> return lastOutput
                else -> error("bad opcode ${mem[ip]}")
            }
        }
    }
}
