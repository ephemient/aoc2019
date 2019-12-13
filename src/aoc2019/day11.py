from aoc2019 import intcode
import asyncio
import fileinput


class Bot:
    def __init__(self, start):
        self.grid = {(0, 0): start}
        self.x = 0
        self.y = 0
        self.dx = 0
        self.dy = 1
        self.turning = False

    async def input(self):
        return int(self.grid.get((self.x, self.y), False))

    async def output(self, value):
        if self.turning:
            if value:
                self.dx, self.dy = self.dy, -self.dx
            else:
                self.dx, self.dy = -self.dy, self.dx
            self.x += self.dx
            self.y += self.dy
            self.turning = False
        else:
            self.grid[(self.x, self.y)] = bool(value)
            self.turning = True


async def walk(mem, start):
    bot = Bot(start)
    await intcode.run_async(mem, bot.input, bot.output)
    return bot.grid


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
