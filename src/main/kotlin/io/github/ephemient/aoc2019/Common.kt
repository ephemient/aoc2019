@file:Suppress("MatchingDeclarationName")
package io.github.ephemient.aoc2019

import java.util.PriorityQueue

fun properMod(a: Long, m: Long): Long {
    check(m > 0)
    val b = a % m
    return if (b < 0) { m + b } else b
}

fun timesMod(a: Long, b: Long, m: Long): Long {
    check(m in 1L until 0x3FFFFFFFFFFFFFFFL)
    check(a in 0L until m)
    check(b in 0L until m)
    if (java.lang.Long.numberOfLeadingZeros(a) + java.lang.Long.numberOfLeadingZeros(b) > 64) {
        return a * b % m
    }
    val ah = a shr 32
    val al = a and 0xFFFFFFFFL
    val bh = b shr 32
    val bl = b and 0xFFFFFFFFL
    var x = java.lang.Long.remainderUnsigned(ah * bh, m)
    repeat(64) { x = java.lang.Long.remainderUnsigned(2 * x, m) }
    var y = java.lang.Long.remainderUnsigned(ah * bl + al * bh, m)
    repeat(32) { y = java.lang.Long.remainderUnsigned(2 * y, m) }
    val z = java.lang.Long.remainderUnsigned(x + al * bl, m)
    return java.lang.Long.remainderUnsigned(y + z, m)
}

fun <T : Comparable<T>> Iterable<T>.minMax(): Pair<T, T>? = minMaxBy(naturalOrder())

fun <T> Iterable<T>.minMaxBy(comparator: Comparator<in T>): Pair<T, T>? {
    val (min, max) = fold(Pair<T?, T?>(null, null)) { (min, max), value ->
        Pair(
            min?.let { minOf(it, value, comparator) } ?: value,
            max?.let { maxOf(it, value, comparator) } ?: value
        )
    }
    return if (min != null && max != null) { min to max } else { null }
}

interface SearchScope<T> {
    val focus: T
    val priority: Int
    fun addNext(value: T, priority: Int = this.priority + 1)
}

inline fun <T> bfs(start: Iterable<T>, visit: SearchScope<T>.() -> Unit) {
    dijkstra(start.associateWithTo(mutableMapOf()) { 0 }, visit)
}

inline fun <T> dijkstra(visited: MutableMap<T, Int>, visit: SearchScope<T>.() -> Unit) {
    val queue = PriorityQueue(visited.size, compareBy(Pair<Int, T>::first))
    for ((value, priority) in visited.entries) {
        queue.add(priority to value)
    }
    while (queue.isNotEmpty()) {
        val (priority, focus) = queue.remove()
        if (visited[focus]?.let { it < priority } == true) {
            continue
        }
        visited[focus] = -1
        object : SearchScope<T> {
            override val focus = focus
            override val priority = priority
            override fun addNext(value: T, priority: Int) {
                if (visited[value]?.let { it > priority } != false) {
                    queue.add(priority to value)
                    visited[value] = priority
                }
            }
        }.visit()
    }
}
