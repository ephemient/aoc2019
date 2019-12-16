package io.github.ephemient.aoc2019

import kotlin.test.Test
import kotlin.test.assertEquals

class Day16Test {
    @Test
    fun `part 1 examples`() {
        assertEquals("12345678", Day16(listOf("12345678")).part1(0))
        assertEquals("48226158", Day16(listOf("12345678")).part1(1))
        assertEquals("34040438", Day16(listOf("12345678")).part1(2))
        assertEquals("03415518", Day16(listOf("12345678")).part1(3))
        assertEquals("01029498", Day16(listOf("12345678")).part1(4))
        assertEquals("24176176", Day16(listOf("80871224585914546619083218645595")).part1())
        assertEquals("73745418", Day16(listOf("19617804207202209144916044189917")).part1())
        assertEquals("52432133", Day16(listOf("69317163492948606335995924319873")).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals("84462026", Day16(listOf("03036732577212944063491565474664")).part2())
        assertEquals("78725270", Day16(listOf("02935109699940807407585447034323")).part2())
        assertEquals("53553731", Day16(listOf("03081770884921959731165446850517")).part2())
    }
}
