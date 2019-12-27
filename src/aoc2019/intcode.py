import asyncio


class Intcode(object):
    def __init__(self, mem, input=()):
        self.mem = mem
        self.base = 0
        self.ip = 0
        self.set_input(input)

    def set_input(self, input):
        if callable(input):
            self._input = input
            return
        try:
            self._input = input.__aiter__().__anext__
            return
        except AttributeError as _:
            pass
        it = iter(input)

        async def aiter():
            try:
                return next(it)
            except StopIteration as e:
                raise StopAsyncIteration(e)

        self._input = aiter

    def _index(self, n):
        mode = self.mem[self.ip] // 10**(n + 1) % 10
        try:
            if mode == 0:
                return self.mem[self.ip + n]
            elif mode == 1:
                return self.ip + n
            elif mode == 2:
                return self.base + self.mem[self.ip + n]
            else:
                raise RuntimeError(f'bad mode {mode}')
        except IndexError as _:
            return 0

    def __getitem__(self, n):
        i = self._index(n)
        return self.mem[i] if i < len(self.mem) else 0

    def __setitem__(self, n, value):
        i = self._index(n)
        if i < len(self.mem):
            self.mem[i] = value
        else:
            self.mem.extend(0 for _ in range(len(self.mem), i))
            self.mem.append(value)

    def __aiter__(self):
        return self

    async def __anext__(self):
        while True:
            op = self.mem[self.ip] % 100
            if op == 1:
                self[3] = self[1] + self[2]
                self.ip += 4
            elif op == 2:
                self[3] = self[1] * self[2]
                self.ip += 4
            elif op == 3:
                self[1] = await self._input()
                self.ip += 2
            elif op == 4:
                output = self[1]
                self.ip += 2
                return output
            elif op == 5:
                self.ip = self[2] if self[1] else self.ip + 3
            elif op == 6:
                self.ip = self[2] if not self[1] else self.ip + 3
            elif op == 7:
                self[3] = int(self[1] < self[2])
                self.ip += 4
            elif op == 8:
                self[3] = int(self[1] == self[2])
                self.ip += 4
            elif op == 9:
                self.base += self[1]
                self.ip += 2
            elif op == 99:
                raise StopAsyncIteration()
            else:
                raise RuntimeError(f'bad opcode {self.mem[self.ip]}')


def run(mem, input=()):
    '''
    >>> run([3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8], [7])
    [0]
    >>> run([3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8], [8])
    [1]
    >>> run([3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8], [9])
    [0]
    >>> run([3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8], [7])
    [1]
    >>> run([3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8], [8])
    [0]
    >>> run([3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8], [9])
    [0]
    >>> run([3, 3, 1108, -1, 8, 3, 4, 3, 99], [7])
    [0]
    >>> run([3, 3, 1108, -1, 8, 3, 4, 3, 99], [8])
    [1]
    >>> run([3, 3, 1108, -1, 8, 3, 4, 3, 99], [9])
    [0]
    >>> run([3, 3, 1107, -1, 8, 3, 4, 3, 99], [7])
    [1]
    >>> run([3, 3, 1107, -1, 8, 3, 4, 3, 99], [8])
    [0]
    >>> run([3, 3, 1107, -1, 8, 3, 4, 3, 99], [9])
    [0]
    >>> run([3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9], [-1])
    [1]
    >>> run([3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9], [0])
    [0]
    >>> run([3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9], [1])
    [1]
    >>> run([3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1], [-1])
    [1]
    >>> run([3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1], [0])
    [0]
    >>> run([3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1], [1])
    [1]
    >>> run([3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99], [7])
    [999]
    >>> run([3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99], [8])
    [1000]
    >>> run([3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99], [9])
    [1001]
    >>> run([109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99], [])
    [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]
    >>> run([1102, 34915192, 34915192, 7, 4, 7, 99, 0], [])
    [1219070632396864]
    >>> run([104, 1125899906842624, 99], [])
    [1125899906842624]
    '''
    async def async_work():
        output = []
        async for value in Intcode(mem, input):
            output.append(value)
        return output

    return asyncio.run(async_work())
