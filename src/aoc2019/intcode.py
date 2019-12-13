import asyncio


class async_next:
    def __init__(self, iterable):
        self.iterator = iter(iterable)

    async def __call__(self):
        return next(self.iterator)


class async_collector:
    def __init__(self):
        self.result = []

    async def __call__(self, value):
        self.result.append(value)


def run(mem, input):
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
        output = async_collector()
        await run_async(mem, async_next(input), output)
        return output.result

    return asyncio.run(async_work())


async def run_async(mem, input, output):
    ip, base = 0, 0
    last_output = None

    def index(n):
        mode = mem[ip] // 10**(n + 1) % 10
        try:
            if mode == 0:
                return mem[ip + n]
            elif mode == 1:
                return ip + n
            elif mode == 2:
                return base + mem[ip + n]
            else:
                raise RuntimeError(f'bad mode {mode}')
        except IndexError as _:
            return 0

    def get_arg(n):
        try:
            return mem[index(n)]
        except IndexError as _:
            return 0

    def set_arg(n, value):
        i = index(n)
        if i < len(mem):
            mem[index(n)] = value
        else:
            mem.extend(0 for _ in range(len(mem), i))
            mem.append(value)

    while True:
        op = mem[ip] % 100
        if op == 1:
            set_arg(3, get_arg(1) + get_arg(2))
            ip += 4
        elif op == 2:
            set_arg(3, get_arg(1) * get_arg(2))
            ip += 4
        elif op == 3:
            set_arg(1, await input())
            ip += 2
        elif op == 4:
            last_output = get_arg(1)
            await output(last_output)
            ip += 2
        elif op == 5:
            ip = get_arg(2) if get_arg(1) else ip + 3
        elif op == 6:
            ip = get_arg(2) if not get_arg(1) else ip + 3
        elif op == 7:
            set_arg(3, int(get_arg(1) < get_arg(2)))
            ip += 4
        elif op == 8:
            set_arg(3, int(get_arg(1) == get_arg(2)))
            ip += 4
        elif op == 9:
            base += get_arg(1)
            ip += 2
        elif op == 99:
            return last_output
        else:
            raise RuntimeError(f'bad opcode {mem[ip]}')
