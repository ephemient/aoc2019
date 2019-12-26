package io.github.ephemient.aoc2019

class Day20(lines: List<String>) {
    private val pass: Set<Pair<Int, Int>> = mutableSetOf<Pair<Int, Int>>().apply {
        for ((y, line) in lines.withIndex()) {
            for ((x, char) in line.withIndex()) {
                if (char == '.') {
                    add(x to y)
                }
            }
        }
    }

    private val portals: Map<Pair<Int, Int>, String>
    init {
        val names = mutableMapOf<Pair<Int, Int>, String>()
        mutableMapOf<Pair<Int, Int>, String>().apply {
            for ((y, line) in lines.withIndex()) {
                for ((x, char) in line.withIndex()) {
                    if (!Character.isUnicodeIdentifierStart(char)) {
                        continue
                    }
                    line.getOrNull(x + 1)
                        ?.takeIf(Character::isUnicodeIdentifierPart)
                        ?.let {
                            names[x to y] = "$char$it"
                            names[x + 1 to y] = "$char$it"
                        }
                    lines.getOrNull(y + 1)
                        ?.getOrNull(x)
                        ?.takeIf(Character::isUnicodeIdentifierPart)
                        ?.let {
                            names[x to y] = "$char$it"
                            names[x to y + 1] = "$char$it"
                        }
                }
            }
        }
        portals = pass.mapNotNull { pos ->
            val (x, y) = pos
            listOf(x - 1 to y, x to y - 1, x to y + 1, x + 1 to y)
                .mapNotNull(names::get)
                .firstOrNull()
                ?.let { pos to it }
        }.toMap()
    }

    private val reversePortals: Map<String, List<Pair<Int, Int>>> = portals.entries.groupBy(
        keySelector = { it.value },
        valueTransform = { it.key }
    )

    @Suppress("ReturnCount")
    fun part1(): Int? {
        bfs(reversePortals["AA"] ?: return null) {
            if (portals[focus] == "ZZ") {
                return priority
            }
            val (x, y) = focus
            for (pos in listOf(x - 1 to y, x to y - 1, x to y + 1, x + 1 to y)) {
                if (pos in pass) {
                    addNext(pos)
                }
            }
            for (pos in portals[focus]?.let(reversePortals::get) ?: return@bfs) {
                addNext(pos)
            }
        }
        return null
    }

    @Suppress("ComplexCondition", "ComplexMethod", "ReturnCount")
    fun part2(): Int? {
        val (minX, maxX) = pass.map(Pair<Int, Int>::first).minMax() ?: return null
        val (minY, maxY) = pass.map(Pair<Int, Int>::second).minMax() ?: return null
        bfs(reversePortals["AA"]?.map { (x, y) -> Triple(x, y, 0) } ?: return null) {
            val (x, y, z) = focus
            if (z == 0 && portals[x to y] == "ZZ") {
                return priority
            }
            for (pos in listOf(x - 1 to y, x to y - 1, x to y + 1, x + 1 to y)) {
                if (pos in pass) {
                    addNext(Triple(pos.first, pos.second, z))
                }
            }
            for (pos in portals[x to y]?.let(reversePortals::get) ?: return@bfs) {
                if (pos.first == x && pos.second == y) {
                    continue
                }
                if (x == minX || x == maxX || y == minY || y == maxY) {
                    if (z > 0) {
                        addNext(Triple(pos.first, pos.second, z - 1))
                    }
                } else if (z < reversePortals.size) {
                    addNext(Triple(pos.first, pos.second, z + 1))
                }
            }
        }
        return null
    }
}
