package io.github.ephemient.aoc2019

import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.sync.Mutex
import kotlinx.coroutines.sync.Semaphore
import kotlinx.coroutines.sync.withLock

class Day23(lines: List<String>) {
    private val ints: List<Long> =
        lines.first().splitToSequence(",").map { it.toLong() }.toList()

    fun part1(): Long = runBlocking {
        val result = Channel<Long>(0)
        val bus = object : Bus() {
            protected override suspend fun sendOutput(i: Int, x: Long, y: Long) {
                if (i == 255) {
                    result.send(y)
                } else {
                    super.sendOutput(i, x, y)
                }
            }
        }
        val jobs = List(bus.n) { i ->
            val input = suspend { bus.getInput(i) }
            launch {
                val vm = Intcode(ints.toMutableList())
                while (true) {
                    bus.putOutput(i, vm.getOutput(input) ?: break)
                }
            }
        }
        result.receive().also {
            for (job in jobs) {
                job.cancel()
            }
        }
    }

    fun part2(): Long = runBlocking {
        val result = Channel<Long>(0)
        var injected: Long? = null
        var natValue: Pair<Long, Long>? = null
        val bus = object : Bus() {
            protected override suspend fun sendOutput(i: Int, x: Long, y: Long) {
                if (i == 255) {
                    natValue = x to y
                } else {
                    super.sendOutput(i, x, y)
                }
            }

            protected override suspend fun onAllBlocked() {
                val (x, y) = checkNotNull(natValue)
                if (y == injected) {
                    result.send(y)
                }
                injected = y
                sendOutput(0, x, y)
            }
        }
        val jobs = List(bus.n) { i ->
            val input = suspend { bus.getInput(i) }
            launch {
                val vm = Intcode(ints.toMutableList())
                while (true) {
                    bus.putOutput(i, vm.getOutput(input) ?: break)
                }
            }
        }
        result.receive().also {
            for (job in jobs) {
                job.cancel()
            }
        }
    }

    private open class Bus(val n: Int = 50) {
        protected val sequentially = Mutex()
        protected val inputs = Array(n) { mutableListOf(it.toLong()) }
        private val outputs = Array(n) { mutableListOf<Long>() }
        protected val polls = IntArray(n)
        private val wakes = Array(n) { Semaphore(1) }

        @Suppress("ReturnCount")
        suspend fun getInput(i: Int): Long {
            val input = inputs[i]
            val allBlocked = sequentially.withLock {
                if (input.isNotEmpty()) {
                    return input.removeAt(0)
                }
                if (polls[i]++ < MAX_POLLS) {
                    return -1
                }
                polls.all { it > MAX_POLLS }
            }
            if (allBlocked) {
                onAllBlocked()
            }
            while (true) {
                wakes[i].acquire()
                sequentially.withLock {
                    if (polls[i] < MAX_POLLS) {
                        check(input.isNotEmpty())
                        return input.removeAt(0)
                    }
                }
            }
        }

        suspend fun putOutput(i: Int, z: Long) {
            val (j, x, y) = sequentially.withLock {
                val output = outputs[i]
                output += z
                if (output.size < 3) {
                    return
                }
                Triple(output.removeAt(0).toInt(), output.removeAt(0), output.removeAt(0))
            }
            sendOutput(j, x, y)
        }

        protected open suspend fun onAllBlocked() {}

        protected open suspend fun sendOutput(i: Int, x: Long, y: Long) {
            val wake = sequentially.withLock {
                val input = inputs[i]
                input += x
                input += y
                polls[i].also { polls[i] = 0 } > MAX_POLLS
            }
            if (wake) {
                wakes[i].release()
            }
        }

        companion object {
            protected const val MAX_POLLS = 1
        }
    }
}
