from aoc2019 import intcode
import fileinput


def part1(lines):
    mem = list(map(int, lines[0].split(',')))
    *_, last = intcode.run(mem, (1, ))
    return last


def part2(lines):
    mem = list(map(int, lines[0].split(',')))
    *_, last = intcode.run(mem, (5, ))
    return last


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
