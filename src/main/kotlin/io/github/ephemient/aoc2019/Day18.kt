package io.github.ephemient.aoc2019

import io.github.ephemient.aoc2019.Day18.Item.Bot
import io.github.ephemient.aoc2019.Day18.Item.Door
import io.github.ephemient.aoc2019.Day18.Item.Key
import io.github.ephemient.aoc2019.Day18.Item.Space

class Day18(lines: List<String>) {
    private val field: Map<Pair<Int, Int>, Item> = mutableMapOf<Pair<Int, Int>, Item>().apply {
        var bots = 0
        for ((y, line) in lines.withIndex()) {
            for ((x, char) in line.withIndex()) {
                when (char) {
                    '@' -> put(x to y, Bot(bots++))
                    in 'A'..'Z' -> put(x to y, Door(Character.toLowerCase(char)))
                    in 'a'..'z' -> put(x to y, Key(char))
                    '.' -> put(x to y, Space)
                }
            }
        }
    }

    private fun paths(
        field: Map<Pair<Int, Int>, Item> = this.field
    ): Map<Item, Map<Key, Pair<Int, Set<Char>>>> =
        field.entries.filter { it.value is Bot || it.value is Key }.associateBy(
            keySelector = { it.value },
            valueTransform = valueTransform@{ (pos, _) ->
                val result = mutableMapOf<Key, Pair<Int, Set<Char>>>()
                val doorsByPos = mutableMapOf(pos to emptySet<Char>())
                bfs(listOf(pos)) {
                    var doors = checkNotNull(doorsByPos[focus])
                    when (val item = field[focus]) {
                        is Key -> {
                            result[item] = priority to doors
                            if (focus != pos) {
                                return@bfs
                            }
                        }
                        is Door -> {
                            doors += item.id
                        }
                    }
                    val (x, y) = focus
                    for (neighbor in listOf(x - 1 to y, x to y - 1, x to y + 1, x + 1 to y)) {
                        if (neighbor !in field || neighbor in doorsByPos) {
                            continue
                        }
                        doorsByPos[neighbor] = doors
                        addNext(neighbor)
                    }
                }
                return@valueTransform result
            }
        )

    private fun solve(paths: Map<Item, Map<Key, Pair<Int, Set<Char>>>>): Int? {
        val allKeys: Set<Char> = paths.keys.mapNotNullTo(mutableSetOf()) { (it as? Key)?.id }
        dijkstra(mutableMapOf(paths.keys.filter { it is Bot } to emptySet<Char>() to 0)) {
            val (items, keys) = focus
            if (keys.size == allKeys.size) {
                return priority
            }
            for ((i, item) in items.withIndex()) {
                for ((key, value) in paths[item]?.entries ?: continue) {
                    val (distance, doors) = value
                    if (keys.containsAll(doors)) {
                        val mutItems = items.toMutableList()
                        mutItems[i] = key
                        addNext(
                            value = mutItems.toList() to keys + key.id,
                            priority = priority + distance
                        )
                    }
                }
            }
        }
        return null
    }

    fun part1(): Int? = solve(paths())

    fun part2(): Int? {
        val (x, y) = field.entries.singleOrNull { it.value is Bot }?.key ?: return null
        val field = field.toMutableMap()
        field -= listOf(x - 1 to y, x to y - 1, x to y, x to y + 1, x + 1 to y)
        field[x - 1 to y - 1] = Bot(0)
        field[x - 1 to y + 1] = Bot(1)
        field[x + 1 to y - 1] = Bot(2)
        field[x + 1 to y + 1] = Bot(3)
        return solve(paths(field))
    }

    private sealed class Item {
        data class Bot(val id: Int) : Item()
        data class Door(val id: Char) : Item()
        data class Key(val id: Char) : Item()
        object Space : Item()
    }
}
