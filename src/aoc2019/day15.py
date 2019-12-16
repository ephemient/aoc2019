from aoc2019 import intcode
import asyncio
from collections import deque
import fileinput


def neighbors(x, y):
    return [((x, y - 1), 1), ((x, y + 1), 2), ((x - 1, y), 3), ((x + 1, y), 4)]


def path(src, dst, free):
    if src == dst:
        return []
    seen = set()
    queue = deque(((src, []), ))
    while True:
        pos, path = queue.popleft()
        for pos2, d in neighbors(*pos):
            if pos2 == dst:
                return path + [(pos2, d)]
            if pos2 in seen or pos2 not in free:
                continue
            queue.append((pos2, path + [(pos2, d)]))
            seen.add(pos2)


async def explore(mem):
    visited = set()
    free = set()
    oxygen = None
    pending = {(0, 0)}
    pos = 0, 0

    input, output = asyncio.Queue(), asyncio.Queue()
    task = asyncio.create_task(intcode.run_async(mem, input.get, output.put))

    while pending:
        target, value = pending.pop(), 1
        visited.add(target)
        for pos2, d in path(pos, target, free):
            await input.put(d)
            value = await output.get()
            if value == 0:
                break
            pos = pos2
            if value == 2:
                oxygen = pos
        if value:
            free.add(pos)
        pending.update(pos2 for pos2, _ in neighbors(*pos)
                       if pos2 not in visited)

    task.cancel()
    return oxygen, free


def part1(lines):
    oxygen, free = asyncio.run(
        explore([int(s.strip()) for s in lines[0].split(',')]))
    return len(path((0, 0), oxygen, free))


def part2(lines):
    oxygen, free = asyncio.run(
        explore([int(s.strip()) for s in lines[0].split(',')]))
    return max(len(path(oxygen, pos, free)) for pos in free)


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
