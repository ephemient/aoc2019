from aoc2019.intcode import Intcode
import asyncio
from collections import deque
import fileinput
from functools import partial

MAX_POLL = 1


async def launch_all(mem, nat, all_blocked=None, n=50):
    lock = asyncio.Lock()
    conds = tuple(asyncio.Condition(lock) for _ in range(n))
    queues = tuple(deque((i, )) for i in range(n))
    polls = [0 for _ in range(n)]

    async def go(mem, i):
        queue, cond = queues[i], conds[i]

        async def input():
            async with lock:
                while True:
                    if queue:
                        polls[i] = 0
                        return queue.popleft()
                    polls[i] += 1
                    if polls[i] <= MAX_POLL:
                        return -1
                    if all(not queues[i] and polls[i] > MAX_POLL
                           for i in range(n)):
                        queues[0].extend(await all_blocked())
                        conds[0].notify()
                    if not queue:
                        await cond.wait()

        aiter = Intcode(mem, input).__aiter__()
        try:
            while True:
                j = await aiter.__anext__()
                async with lock:
                    polls[i] = 0
                x = await aiter.__anext__()
                async with lock:
                    polls[i] = 0
                y = await aiter.__anext__()
                async with lock:
                    polls[i] = 0
                if j == 255:
                    await nat(x, y)
                else:
                    async with lock:
                        queues[j].extend((x, y))
                        conds[j].notify()
        except StopAsyncIteration as _:
            pass

    return [asyncio.create_task(go(mem[:], i)) for i in range(n)]


def part1(lines):
    async def async_work():
        result = asyncio.Queue()

        async def nat(x, y):
            await result.put(y)

        tasks = await launch_all([int(s.strip()) for s in lines[0].split(',')],
                                 nat)
        try:
            return await result.get()
        finally:
            for task in tasks:
                task.cancel()

    return asyncio.run(async_work())


def part2(lines):
    async def async_work():
        result = asyncio.Queue()
        nat_value = []
        last_y = []

        async def nat(x, y):
            nat_value[:] = [(x, y)]

        async def all_blocked():
            x, y = nat_value[0]
            if [y] == last_y:
                await result.put(y)
            last_y[:] = [y]
            return x, y

        tasks = await launch_all([int(s.strip()) for s in lines[0].split(',')],
                                 nat, all_blocked)
        try:
            return await result.get()
        finally:
            for task in tasks:
                task.cancel()

    return asyncio.run(async_work())


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
