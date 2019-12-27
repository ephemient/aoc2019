from aoc2019.intcode import Intcode
import asyncio
import fileinput
from itertools import permutations


async def run_async(mem, input, output):
    async for value in Intcode(mem, input):
        await output(value)
    return value


async def amplify(mem, order):
    '''
    >>> asyncio.run(amplify([3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0], (4, 3, 2, 1, 0)))
    43210
    >>> asyncio.run(amplify([3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0], (0, 1, 2, 3, 4)))
    54321
    >>> asyncio.run(amplify([3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0], (1, 0, 4, 3, 2)))
    65210
    >>> asyncio.run(amplify([3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5], (9, 8, 7, 6, 5)))
    139629729
    >>> asyncio.run(amplify([3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54, -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4, 53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10], (9, 7, 8, 5, 6)))
    18216
    '''
    channels = [asyncio.Queue() for _ in order]
    for n, channel in zip(order, channels):
        await channel.put(n)
    await channels[0].put(0)
    for input, output in zip(channels, channels[1:]):
        asyncio.create_task(run_async(mem[:], input.get, output.put))
    return await run_async(mem[:], channels[-1].get, channels[0].put)


async def max_order(mem, orders):
    values = await asyncio.gather(*(amplify(mem, order) for order in orders))
    return max(values)


def part1(lines):
    mem = list(map(int, lines[0].split(',')))
    return asyncio.run(max_order(mem, permutations(range(5))))


def part2(lines):
    mem = list(map(int, lines[0].split(',')))
    return asyncio.run(max_order(mem, permutations(range(5, 10))))


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
