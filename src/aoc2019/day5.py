import fileinput


def run(mem, input):
    '''
    >>> list(run([3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8], lambda: 7))
    [0]
    >>> list(run([3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8], lambda: 8))
    [1]
    >>> list(run([3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8], lambda: 9))
    [0]
    >>> list(run([3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8], lambda: 7))
    [1]
    >>> list(run([3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8], lambda: 8))
    [0]
    >>> list(run([3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8], lambda: 9))
    [0]
    >>> list(run([3, 3, 1108, -1, 8, 3, 4, 3, 99], lambda: 7))
    [0]
    >>> list(run([3, 3, 1108, -1, 8, 3, 4, 3, 99], lambda: 8))
    [1]
    >>> list(run([3, 3, 1108, -1, 8, 3, 4, 3, 99], lambda: 9))
    [0]
    >>> list(run([3, 3, 1107, -1, 8, 3, 4, 3, 99], lambda: 7))
    [1]
    >>> list(run([3, 3, 1107, -1, 8, 3, 4, 3, 99], lambda: 8))
    [0]
    >>> list(run([3, 3, 1107, -1, 8, 3, 4, 3, 99], lambda: 9))
    [0]
    >>> list(run([3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9], lambda: -1))
    [1]
    >>> list(run([3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9], lambda: 0))
    [0]
    >>> list(run([3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9], lambda: 1))
    [1]
    >>> list(run([3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1], lambda: -1))
    [1]
    >>> list(run([3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1], lambda: 0))
    [0]
    >>> list(run([3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1], lambda: 1))
    [1]
    >>> list(run([3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99], lambda: 7))
    [999]
    >>> list(run([3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99], lambda: 8))
    [1000]
    >>> list(run([3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99], lambda: 9))
    [1001]
    '''
    ip = 0

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
            set_arg(1, input())
            ip += 2
        elif op == 4:
            yield get_arg(1)
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
            return
        else:
            raise RuntimeError(f'bad opcode {mem[ip]}')


def part1(lines):
    mem = list(map(int, lines[0].split(',')))
    *_, last = run(mem, lambda: 1)
    return last


def part2(lines):
    mem = list(map(int, lines[0].split(',')))
    *_, last = run(mem, lambda: 5)
    return last


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
