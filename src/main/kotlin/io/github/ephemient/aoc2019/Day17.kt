package io.github.ephemient.aoc2019

class Day17(lines: List<String>) {
    private val ints: List<Long> =
        lines.first().splitToSequence(",").map { it.toLong() }.toList()

    private val field: Set<Pair<Int, Int>>
    private val startPosition: Pair<Int, Int>
    private val startDirection: Pair<Int, Int>
    init {
        var x = 0
        var y = 0
        val field = mutableSetOf<Pair<Int, Int>>()
        var startPosition: Pair<Int, Int>? = null
        var startDirection: Pair<Int, Int>? = null
        for (int in Intcode(ints.toMutableList()).runBlocking(emptyList())) {
            when (int.toChar()) {
                '#' -> field.add(x to y)
                '<' -> {
                    startPosition = x to y
                    startDirection = -1 to 0
                }
                '>' -> {
                    startPosition = x to y
                    startDirection = 1 to 0
                }
                '^' -> {
                    startPosition = x to y
                    startDirection = 0 to -1
                }
                'v' -> {
                    startPosition = x to y
                    startDirection = 0 to 1
                }
            }
            if (int.toChar() == '\n') {
                x = 0
                y++
            } else {
                x++
            }
        }
        this.field = field
        this.startPosition = checkNotNull(startPosition)
        this.startDirection = checkNotNull(startDirection)
    }

    private val crossings: Set<Pair<Int, Int>> = field.filterTo(mutableSetOf()) { (x, y) ->
        listOf(x - 1 to y, x to y - 1, x to y + 1, x + 1 to y).count(field::contains) > 2
    }

    fun part1(): Int = crossings.sumBy { (x, y) -> x * y }

    @Suppress("ComplexMethod", "LongMethod", "LoopWithTooManyJumpStatements")
    fun part2(): Long? {
        val paths = sequence<List<String>> {
            val stack = mutableListOf(
                PathState(true, 0, startPosition, startDirection, mutableListOf(), mutableSetOf())
            )
            while (stack.isNotEmpty()) {
                val state = stack.removeAt(stack.lastIndex)
                var (canTurn, consecutive) = state
                var (x, y) = state.position
                val (dx, dy) = state.direction
                val parts = state.parts
                val seen = state.seen.toMutableSet()
                while (true) {
                    if (canTurn) {
                        stack += PathState(
                            false,
                            0,
                            x to y,
                            dy to -dx,
                            parts + listOfNotNull(consecutive.takeIf { it > 0 }?.toString(), "L"),
                            seen.toSet()
                        )
                        stack += PathState(
                            false,
                            0,
                            x to y,
                            -dy to dx,
                            parts + listOfNotNull(consecutive.takeIf { it > 0 }?.toString(), "R"),
                            seen.toSet()
                        )
                    } else {
                        canTurn = true
                    }
                    consecutive++
                    x += dx
                    y += dy
                    if (x to y !in field || !seen.add(x to y) && x to y !in crossings) {
                        break
                    }
                    if (seen.size == field.size) {
                        yield(parts + consecutive.toString())
                        break
                    }
                }
            }
        }
        val compressed = paths.mapNotNull { path ->
            val stack = mutableListOf(CompressState(emptyList(), emptyList(), 0))
            while (stack.isNotEmpty()) {
                val state = stack.removeAt(stack.lastIndex)
                if (state.index >= path.size) {
                    return@mapNotNull state
                }
                val (main, programs, index) = state
                if (main.size < 10) {
                    for ((i, program) in programs.withIndex()) {
                        if (path.subList(index, minOf(path.size, index + program.size)) ==
                            program.subList(0, minOf(program.size, path.size - index))
                        ) {
                            stack += CompressState(
                                main + i,
                                programs,
                                index + program.size
                            )
                        }
                    }
                }
                if (programs.size < 3) {
                    for (end in index + 1 until path.size) {
                        val program = path.subList(index, end)
                        if (program.sumBy { it.length } + program.size - 1 > 20) {
                            break
                        }
                        stack += CompressState(
                            main + programs.size,
                            programs.plusElement(program),
                            index + program.size
                        )
                    }
                }
            }
            return@mapNotNull null
        }.firstOrNull() ?: return null
        val input = listOf(
            compressed.main.joinToString(",") { ('A' + it).toString() },
            compressed.programs[0].joinToString(","),
            compressed.programs[1].joinToString(","),
            compressed.programs[2].joinToString(","),
            "n"
        ).joinToString("") { "$it\n" }
        return Intcode(ints.toMutableList().apply { set(0, 2) })
            .runBlocking(input.map { it.toLong() })
            .firstOrNull { it > 255 }
    }

    private data class PathState(
        val canTurn: Boolean,
        val consecutive: Int,
        val position: Pair<Int, Int>,
        val direction: Pair<Int, Int>,
        val parts: List<String>,
        val seen: Set<Pair<Int, Int>>
    )

    private data class CompressState(
        val main: List<Int>,
        val programs: List<List<String>>,
        val index: Int
    )
}
