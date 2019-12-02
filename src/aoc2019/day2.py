import fileinput


def step(mem, ip):
    '''
    >>> mem = [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]
    >>> step(mem, 0)
    4
    >>> mem
    [1, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
    >>> step(mem, 4)
    8
    >>> mem
    [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
    >>> step(mem, 8)
    >>> mem
    [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
    '''
    op = mem[ip]
    if op == 1:
        mem[mem[ip + 3]] = mem[mem[ip + 1]] + mem[mem[ip + 2]]
        return ip + 4
    elif op == 2:
        mem[mem[ip + 3]] = mem[mem[ip + 1]] * mem[mem[ip + 2]]
        return ip + 4
    elif op == 99:
        return None
    else:
        raise RuntimeError('unhandled opcode')


def run(mem):
    '''
    >>> mem = [1, 0, 0, 0, 99]
    >>> run(mem)
    >>> mem
    [2, 0, 0, 0, 99]
    >>> mem = [2, 3, 0, 3, 99]
    >>> run(mem)
    >>> mem
    [2, 3, 0, 6, 99]
    >>> mem = [2, 4, 4, 5, 99, 0]
    >>> run(mem)
    >>> mem
    [2, 4, 4, 5, 99, 9801]
    >>> mem = [1, 1, 1, 4, 99, 5, 6, 0, 99]
    >>> run(mem)
    >>> mem
    [30, 1, 1, 4, 2, 5, 6, 0, 99]
    '''
    ip = 0
    while ip is not None:
        ip = step(mem, ip)


def part1(lines):
    mem = list(map(int, lines[0].split(',')))
    mem[1] = 12
    mem[2] = 2
    run(mem)
    return mem[0]


def part2(lines):
    mem0 = list(map(int, lines[0].split(',')))
    for noun in range(100):
        for verb in range(100):
            mem = mem0[:]
            mem[1] = noun
            mem[2] = verb
            run(mem)
            if mem[0] == 19690720:
                return 100 * noun + verb


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
