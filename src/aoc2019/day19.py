from aoc2019 import intcode
import fileinput


def part1(lines):
    mem = [int(s.strip()) for s in lines[0].split(',')]
    return sum(
        intcode.run(mem[:], (x, y))[0] for x in range(50) for y in range(50))


def part2(lines):
    mem = [int(s.strip()) for s in lines[0].split(',')]
    x, y = 0, 99
    while True:
        while not intcode.run(mem[:], (x, y))[0]:
            x += 1
        if intcode.run(mem[:], (x + 99, y - 99))[0]:
            return 10000 * x + y - 99
        y += 1


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
