package io.github.ephemient.aoc2019

import kotlin.test.Test
import kotlin.test.assertEquals

class Day18Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(8, Day18(SAMPLE_1).part1())
        assertEquals(86, Day18(SAMPLE_2).part1())
        assertEquals(132, Day18(SAMPLE_3).part1())
        assertEquals(136, Day18(SAMPLE_4).part1())
        assertEquals(81, Day18(SAMPLE_5).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(8, Day18(SAMPLE_6).part2())
        assertEquals(24, Day18(SAMPLE_7).part2())
        assertEquals(32, Day18(SAMPLE_8).part2())
        assertEquals(72, Day18(SAMPLE_9).part2())
    }

    companion object {
        private val SAMPLE_1 =
            """
            #########
            #b.A.@.a#
            #########
            """.trimIndent().lines()
        private val SAMPLE_2 =
            """
            ########################
            #f.D.E.e.C.b.A.@.a.B.c.#
            ######################.#
            #d.....................#
            ########################
            """.trimIndent().lines()
        private val SAMPLE_3 =
            """
            ########################
            #...............b.C.D.f#
            #.######################
            #.....@.a.B.c.d.A.e.F.g#
            ########################
            """.trimIndent().lines()
        private val SAMPLE_4 =
            """
            #################
            #i.G..c...e..H.p#
            ########.########
            #j.A..b...f..D.o#
            ########@########
            #k.E..a...g..B.n#
            ########.########
            #l.F..d...h..C.m#
            #################
            """.trimIndent().lines()
        private val SAMPLE_5 =
            """
            ########################
            #@..............ac.GI.b#
            ###d#e#f################
            ###A#B#C################
            ###g#h#i################
            ########################
            """.trimIndent().lines()
        private val SAMPLE_6 =
            """
            #######
            #a.#Cd#
            ##...##
            ##.@.##
            ##...##
            #cB#Ab#
            #######
            """.trimIndent().lines()
        private val SAMPLE_7 =
            """
            ###############
            #d.ABC.#.....a#
            ######...######
            ######.@.######
            ######...######
            #b.....#.....c#
            ###############
            """.trimIndent().lines()
        private val SAMPLE_8 =
            """
            #############
            #DcBa.#.GhKl#
            #.###...#I###
            #e#d#.@.#j#k#
            ###C#...###J#
            #fEbA.#.FgHi#
            #############
            """.trimIndent().lines()
        private val SAMPLE_9 =
            """
            #############
            #g#f.D#..h#l#
            #F###e#E###.#
            #dCba...BcIJ#
            #####.@.#####
            #nK.L...G...#
            #M###N#H###.#
            #o#m..#i#jk.#
            #############
            """.trimIndent().lines()
    }
}
