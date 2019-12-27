from aoc2019 import intcode
import fileinput
import textwrap

SPRINGCODE_1 = textwrap.dedent("""
    OR A T
    AND B T
    AND C T
    NOT T J
    AND D J
    WALK
    """).lstrip()
SPRINGCODE_2 = textwrap.dedent("""
    OR A T
    AND B T
    AND C T
    NOT T J
    AND D J
    NOT J T
    OR E T
    OR H T
    AND T J
    RUN
    """).lstrip()


def part1(lines):
    return next(x for x in intcode.run(
        [int(s.strip())
         for s in lines[0].split(',')], list(map(ord, SPRINGCODE_1)))
                if x > 255)


def part2(lines):
    return next(x for x in intcode.run(
        [int(s.strip())
         for s in lines[0].split(',')], list(map(ord, SPRINGCODE_2)))
                if x > 255)


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
