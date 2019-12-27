from aoc2019 import intcode
from aoc2019.intcode import Intcode
import asyncio
import fileinput


def part1(lines):
    blocks = set()
    for [x, y, value] in zip(
            *
        [iter(intcode.run([int(s.strip())
                           for s in lines[0].split(',')]))] * 3):
        if value == 2:
            blocks.add((x, y))
        else:
            blocks.discard((x, y))
    return len(blocks)


async def part2_async(lines):
    mem = [int(s.strip()) for s in lines[0].split(',')]
    mem[0] = 2

    async def input():
        return -1 if ball < paddle else 1 if ball > paddle else 0

    aiter, score = Intcode(mem, input).__aiter__(), None
    try:
        while True:
            x = await aiter.__anext__()
            y = await aiter.__anext__()
            value = await aiter.__anext__()
            if x == -1 and y == 0:
                score = value
            elif value == 3:
                paddle = x
            elif value == 4:
                ball = x
    except StopAsyncIteration as _:
        return score


def part2(lines):
    return asyncio.run(part2_async(lines))


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
