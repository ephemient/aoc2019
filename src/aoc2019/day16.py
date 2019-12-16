import fileinput
from functools import reduce


def part1(lines, times=100):
    '''
    >>> part1(["12345678"], 0)
    '12345678'
    >>> part1(["12345678"], 1)
    '48226158'
    >>> part1(["12345678"], 2)
    '34040438'
    >>> part1(["12345678"], 3)
    '03415518'
    >>> part1(["12345678"], 4)
    '01029498'
    >>> part1(["80871224585914546619083218645595"])
    '24176176'
    >>> part1(["19617804207202209144916044189917"])
    '73745418'
    >>> part1(["69317163492948606335995924319873"])
    '52432133'
    '''
    value = list(map(int, lines[0].strip()))
    for _ in range(times):
        value = [
            abs(
                sum(x * (0, 1, 0, -1)[(j + 1) % (4 * i + 4) // (i + 1)]
                    for j, x in enumerate(value))) % 10
            for i in range(len(value))
        ]
    return ''.join(map(str, value[:8]))


def part2(lines, times=100):
    '''
    >>> part2(["03036732577212944063491565474664"])
    '84462026'
    >>> part2(["02935109699940807407585447034323"])
    '78725270'
    >>> part2(["03081770884921959731165446850517"])
    '53553731'
    '''
    value = list(map(int, lines[0].strip()))
    offset = reduce(lambda acc, x: 10 * acc + x, value[:7], 0)
    logical_len = 10000 * len(value)
    assert offset < logical_len <= 2 * offset
    value = [value[i % len(value)] for i in range(offset, logical_len)]
    for _ in range(times):
        for i in reversed(range(1, len(value))):
            value[i - 1] += value[i]
        for i in range(len(value)):
            value[i] = abs(value[i]) % 10
    return ''.join(map(str, value[:8]))


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
