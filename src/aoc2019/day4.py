import fileinput


def generate_non_decreasing(lo, hi):
    assert len(lo) == len(hi)
    current, stop = list(map(int, lo)), list(map(int, hi))
    for i in range(len(current) - 1):
        current[i + 1] = max(current[i], current[i + 1])
    while current <= stop:
        yield current
        n = next(i + 1 for i, c in enumerate(reversed(current)) if c < 9)
        current[-n:] = n * (current[-n] + 1, )


def part1(lines):
    '''
    >>> part1(["111111-111111"])
    1
    >>> part1(["223450-223450"])
    0
    >>> part1(["123789-123789"])
    0
    '''
    count = 0
    for a in generate_non_decreasing(*(s.strip()
                                       for s in lines[0].split('-'))):
        for i in range(len(a) - 1):
            if a[i] == a[i + 1]:
                count += 1
                break
    return count


def part2(lines):
    '''
    >>> part2(["112233-112233"])
    1
    >>> part2(["123444-123444"])
    0
    >>> part2(["111122-111122"])
    1
    '''
    count = 0
    for a in generate_non_decreasing(*(s.strip()
                                       for s in lines[0].split('-'))):
        for i in range(len(a) - 1):
            if a[i] == a[i + 1] and (i <= 0 or a[i - 1] != a[i]) and (
                    i + 2 >= len(a) or a[i + 1] != a[i + 2]):
                count += 1
                break
    return count


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
