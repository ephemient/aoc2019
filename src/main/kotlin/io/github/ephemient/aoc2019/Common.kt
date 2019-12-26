@file:Suppress("MatchingDeclarationName")
package io.github.ephemient.aoc2019

import java.util.PriorityQueue

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
