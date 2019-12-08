package io.github.ephemient.aoc2019

import com.google.common.math.IntMath
import kotlinx.coroutines.async
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.channels.ReceiveChannel
import kotlinx.coroutines.channels.SendChannel
import kotlinx.coroutines.channels.toList
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking

class Intcode(private val mem: IntArray) {
    private fun arg(ip: Int, n: Int): Int =
        if (mem[ip] / IntMath.pow(10, n + 1) % 10 == 0) {
            mem[ip + n]
        } else {
            ip + n
        }

    private operator fun IntArray.get(ip: Int, n: Int): Int = mem[arg(ip, n)]

    private operator fun IntArray.set(ip: Int, n: Int, value: Int) {
        mem[arg(ip, n)] = value
    }

    fun runBlocking(input: Iterable<Int>): List<Int> = runBlocking {
        val inputChannel = Channel<Int>()
        val outputChannel = Channel<Int>()
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
    suspend fun runAsync(input: ReceiveChannel<Int>, output: SendChannel<Int>): Int? {
        var ip = 0
        var lastOutput: Int? = null
        while (true) {
            when (mem[ip] % 100) {
                1 -> {
                    mem[ip, 3] = mem[ip, 1] + mem[ip, 2]
                    ip += 4
                }
                2 -> {
                    mem[ip, 3] = mem[ip, 1] * mem[ip, 2]
                    ip += 4
                }
                3 -> {
                    mem[ip, 1] = input.receive()
                    ip += 2
                }
                4 -> {
                    output.send(mem[ip, 1].also { lastOutput = it })
                    ip += 2
                }
                5 -> ip = if (mem[ip, 1] != 0) mem[ip, 2] else ip + 3
                6 -> ip = if (mem[ip, 1] == 0) mem[ip, 2] else ip + 3
                7 -> {
                    mem[ip, 3] = if (mem[ip, 1] < mem[ip, 2]) 1 else 0
                    ip += 4
                }
                8 -> {
                    mem[ip, 3] = if (mem[ip, 1] == mem[ip, 2]) 1 else 0
                    ip += 4
                }
                99 -> return lastOutput
                else -> error("unhandled opcode ${mem[ip]}")
            }
        }
    }
}
