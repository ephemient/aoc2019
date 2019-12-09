import fileinput
from aoc2019 import intcode


def part1(lines):
    mem = list(map(int, lines[0].split(',')))
    *_, last = intcode.run(mem, (1, ))
    return last


def part2(lines):
    mem = list(map(int, lines[0].split(',')))
    *_, last = intcode.run(mem, (2, ))
    return last


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
