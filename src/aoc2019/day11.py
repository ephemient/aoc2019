from aoc2019 import intcode
import asyncio
from collections import defaultdict
import fileinput


async def walk(mem, start):
    input = asyncio.Queue()
    output = asyncio.Queue()

    async def work():
        grid = defaultdict(bool)
        grid[(0, 0)] = start
        x, y = 0, 0
        dx, dy = 0, 1
        while True:
            await input.put(int(grid[(x, y)]))
            color = await output.get()
            if color is None:
                break
            grid[(x, y)] = bool(color)
            turn = await output.get()
            if turn is None:
                break
            else:
                dx, dy = (dy, -dx) if turn else (-dy, dx)
            x += dx
            y += dy
        return grid

    result = asyncio.create_task(work())
    await intcode.run_async(mem, input, output)
    await output.put(None)
    return (await asyncio.gather(result))[0]


def part1(lines):
    '''
    >>> part1(["3,0,5,0,-1,104,1,104,0,3,0,5,0,-1,104,0,104,0,3,0,104,1,104,0,3,0,104,1,104,0,3,0,6,0,-1,104,0,104,1,3,0,104,1,104,0,3,0,104,1,104,0,99"])
    6
    '''
    return len(
        asyncio.run(walk([int(s.strip()) for s in lines[0].split(',')],
                         False)))


def part2(lines):
    result = [
        k for k, v in asyncio.run(
            walk([int(s.strip()) for s in lines[0].split(',')], True)).items()
        if v
    ]
    return '\n'.join(''.join('\u2591\u2593'[int((x, y) in result)]
                             for x in range(min(k[0] for k in result),
                                            max(k[0] for k in result) + 1))
                     for y in range(max(k[1] for k in result),
                                    min(k[1] for k in result) - 1, -1))


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
