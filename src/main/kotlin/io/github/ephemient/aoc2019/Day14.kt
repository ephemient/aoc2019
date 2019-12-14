package io.github.ephemient.aoc2019

class Day14(lines: List<String>) {
    private val rules: Map<String, Pair<Long, Map<String, Long>>> = lines.associate { line ->
        val items = PATTERN.findAll(line).map { match ->
            val (n, id) = match.destructured
            id to n.toLong()
        }.toMutableList()
        val (id, n) = items.removeAt(items.lastIndex)
        id to (n to items.toMap())
    }

    private fun produce(quantity: Long): Long {
        val mats = mutableMapOf("FUEL" to quantity)
        while (true) {
            val (key, n) = mats.entries.find { (key, n) -> key != "ORE" && n > 0 } ?: break
            val (m, srcs) = checkNotNull(rules[key]) { "null key=$key" }
            val x = (n + m - 1) / m
            mats[key] = n - m * x
            for ((k, v) in srcs) {
                mats[k] = mats.getOrElse(k) { 0 } + x * v
            }
        }
        return mats.getOrElse("ORE") { 0 }
    }

    fun part1(): Long = produce(1)

    fun part2(): Long {
        var good = MAX / produce(1)
        var bad: Long? = null
        while (bad == null || good < bad - 1) {
            val quantity = bad?.let { (good + it) / 2 } ?: good * 2
            val ore = produce(quantity)
            if (ore < MAX) {
                good = quantity
            } else if (ore == MAX) {
                return quantity
            } else {
                bad = quantity
            }
        }
        return good
    }

    companion object {
        private val PATTERN =
            """(\d+) (\w+)""".toRegex()
        private const val MAX = 1_000_000_000_000
    }
}
