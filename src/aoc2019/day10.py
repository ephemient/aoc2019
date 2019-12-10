from collections import defaultdict
import fileinput
from fractions import Fraction
from itertools import cycle, islice
import math


def parse(lines):
    return [(x, y) for y, line in enumerate(lines) for x, c in enumerate(line)
            if c == '#']


def best(points):
    point, result = max(((point, explore(point, points)) for point in points),
                        key=lambda entry: len(entry[1]))
    x, y = point
    for value in result.values():
        value.sort(key=lambda other: abs(x - other[0]) + abs(y - other[1]))
    return result


def explore(point, points):
    result = defaultdict(list)
    for other in points:
        if point != other:
            result[direction(point, other)].append(other)
    return result


def direction(src, dst):
    x, y = src
    u, v = dst
    return (y < v, -math.inf) if x == u else (x > u, Fraction(y - v, x - u))


def roundrobin(*iterables):
    "roundrobin('ABC', 'D', 'EF') --> A D E B F C"
    # Recipe credited to George Sakkis
    num_active = len(iterables)
    nexts = cycle(iter(it).__next__ for it in iterables)
    while num_active:
        try:
            for next in nexts:
                yield next()
        except StopIteration:
            # Remove the iterator we just exhausted from the cycle.
            num_active -= 1
            nexts = cycle(islice(nexts, num_active))


def part1(lines):
    '''
    >>> part1(".#..# ..... ##### ....# ...##".split())
    8
    >>> part1("......#.#. #..#.#.... ..#######. .#.#.###.. .#..#..... ..#....#.# #..#....#. .##.#..### ##...#..#. .#....####".split())
    33
    >>> part1("#.#...#.#. .###....#. .#....#... ##.#.#.#.# ....#.#.#. .##..###.# ..#...##.. ..##....## ......#... .####.###.".split())
    35
    >>> part1(".#..#..### ####.###.# ....###.#. ..###.##.# ##.##.#.#. ....###..# ..#.#..#.# #..#.#.### .##...##.# .....#.#..".split())
    41
    >>> part1(".#..##.###...####### ##.############..##. .#.######.########.# .###.#######.####.#. #####.##.#.##.###.## ..#####..#.######### #################### #.####....###.#.#.## ##.################# #####.##.###..####.. ..######..##.####### ####.##.####...##..# .#####..#.######.### ##...#.##########... #.##########.####### .####.#.###.###.#.## ....##.##.###..##### .#.#.###########.### #.#.#.#####.####.### ###.##.####.##.#..##".split())
    210
    '''
    return len(best(parse(lines)))


def part2(lines, n=200):
    x, y = next(
        islice(
            roundrobin(*(points
                         for _, points in sorted(best(parse(lines)).items(),
                                                 key=lambda entry: entry[0]))),
            n - 1, n))
    return 100 * x + y


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
