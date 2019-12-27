from aoc2019 import intcode
import fileinput


def part1(lines):
    mem = list(map(int, lines[0].split(',')))
    mem[1] = 12
    mem[2] = 2
    intcode.run(mem)
    return mem[0]


def part2(lines):
    mem0 = list(map(int, lines[0].split(',')))
    for noun in range(100):
        for verb in range(100):
            mem = mem0[:]
            mem[1] = noun
            mem[2] = verb
            intcode.run(mem)
            if mem[0] == 19690720:
                return 100 * noun + verb


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
