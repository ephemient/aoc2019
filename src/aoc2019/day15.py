from aoc2019.intcode import Intcode
import asyncio
from collections import deque
import fileinput


def neighbors(x, y):
    return [((x, y - 1), 1), ((x, y + 1), 2), ((x - 1, y), 3), ((x + 1, y), 4)]


def path(free, src, dst):
    queue, seen = deque(((src, []), )), {src}
    while queue:
        pos, path = queue.popleft()
        for pos2, d in neighbors(*pos):
            if pos2 == dst:
                return path + [(pos2, d)]
            if pos2 in seen or pos2 not in free:
                continue
            queue.append((pos2, path + [(pos2, d)]))
            seen.add(pos2)
    return path


async def explore(mem):
    visited = set()
    free = set()
    oxygen = None
    pending = {(0, 0)}
    pos = 0, 0
    vm = Intcode(mem)
    aiter = vm.__aiter__()
    while pending:
        target, value = pending.pop(), 1
        visited.add(target)
        for pos2, d in path(free, pos, target):
            vm.set_input((d, ))
            value = await aiter.__anext__()
            if value == 0:
                break
            pos = pos2
            if value == 2:
                oxygen = pos
        if value:
            free.add(pos)
        pending.update(pos2 for pos2, _ in neighbors(*pos)
                       if pos2 not in visited)
    return oxygen, free


def part1(lines):
    oxygen, free = asyncio.run(
        explore([int(s.strip()) for s in lines[0].split(',')]))
    return len(path(free, (0, 0), oxygen))


def part2(lines):
    oxygen, free = asyncio.run(
        explore([int(s.strip()) for s in lines[0].split(',')]))
    return len(path(free, oxygen, None))


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
