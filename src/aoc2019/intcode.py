import asyncio


def run(mem, raw_input):
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
    '''
    async def async_work():
        input, output = asyncio.Queue(), asyncio.Queue()
        for value in raw_input:
            await input.put(value)
        await run_async(mem, input, output)
        raw_output = []
        while not output.empty():
            raw_output.append(await output.get())
        return raw_output
    return asyncio.run(async_work())


async def run_async(mem, input, output):
    ip = 0
    last_output = None

    def get_arg(n):
        if mem[ip] // 10**(n + 1) % 10 == 0:
            return mem[mem[ip + n]]
        else:
            return mem[ip + n]

    def set_arg(n, value):
        mem[mem[ip + n]] = value

    while True:
        op = mem[ip] % 100
        if op == 1:
            set_arg(3, get_arg(1) + get_arg(2))
            ip += 4
        elif op == 2:
            set_arg(3, get_arg(1) * get_arg(2))
            ip += 4
        elif op == 3:
            set_arg(1, await input.get())
            ip += 2
        elif op == 4:
            last_output = get_arg(1)
            await output.put(last_output)
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
        elif op == 99:
            return last_output
        else:
            raise RuntimeError(f'bad opcode {mem[ip]}')
