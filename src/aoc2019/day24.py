from collections import Counter
import fileinput


def parse(lines):
    return frozenset((x, y) for y in range(-2, 3) for x in range(-2, 3)
                     if lines[y + 2][x + 2] == '#')


def evolve(bugs, neighbors):
    counter = Counter()
    for bug in bugs:
        counter.update(neighbors(*bug))
    return frozenset(bug for bug, count in counter.items()
                     if count == 1 or count == 2 and bug not in bugs)


def part1(lines):
    '''
    >>> part1("....# #..#. #..## ..#.. #....".split())
    2129920
    '''
    def neighbors(x, y):
        return list(
            filter(None, ((x - 1, y) if x > -2 else None,
                          (x, y - 1) if y > -2 else None,
                          (x + 1, y) if x < 2 else None,
                          (x, y + 1) if y < 2 else None)))

    seen = set()
    bugs = parse(lines)
    while bugs not in seen:
        seen.add(bugs)
        bugs = evolve(bugs, neighbors)
    return sum(1 << x + 5 * y + 12 for y in range(-2, 3) for x in range(-2, 3)
               if (x, y) in bugs)


def part2(lines, n=200):
    '''
    >>> part2("....# #..#. #..## ..#.. #....".split(), 10)
    99
    '''
    def neighbors(x, y, z):
        neighbors = []
        if x > -2 and (x, y) != (1, 0): neighbors.append((x - 1, y, z))
        if y > -2 and (x, y) != (0, 1): neighbors.append((x, y - 1, z))
        if x < 2 and (x, y) != (-1, 0): neighbors.append((x + 1, y, z))
        if y < 2 and (x, y) != (0, -1): neighbors.append((x, y + 1, z))
        if (abs(x), y) == (1, 0):
            neighbors.extend((2 * x, y, z - 1) for y in range(-2, 3))
        if (x, abs(y)) == (0, 1):
            neighbors.extend((x, 2 * y, z - 1) for x in range(-2, 3))
        if abs(x) == 2: neighbors.append((x // 2, 0, z + 1))
        if abs(y) == 2: neighbors.append((0, y // 2, z + 1))
        return neighbors

    bugs = frozenset((x, y, 0) for x, y in parse(lines))
    for _ in range(n):
        bugs = evolve(bugs, neighbors)
    return len(bugs)


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
