import fileinput
import itertools
import operator
import re

PATTERN = re.compile(r'-?\d+')


def lcm(x, y):
    a, b = x, y
    while a:
        a, b = b % a, a
    return x // b * y


def simulate(initialState):
    points = [point for point, _ in initialState]
    velocities = [velocity for _, velocity in initialState]
    while True:
        for i, p1 in enumerate(points):
            for p2 in points:
                if p1 < p2:
                    velocities[i] += 1
                elif p1 > p2:
                    velocities[i] -= 1
        for i, velocity in enumerate(velocities):
            points[i] += velocity
        yield list(zip(points, velocities))


def parse(lines):
    axes = [[], [], []]
    for line in lines:
        for i, match in enumerate(PATTERN.finditer(line)):
            axes[i].append((int(match.group()), 0))
    return axes


def part1(lines, n=1000):
    '''
    >>> part1(["<x=-1, y=0, z=2>", "<x=2, y=-10, z=-7>", "<x=4, y=-8, z=8>", "<x=3, y=5, z=-1>"], 10)
    179
    >>> part1(["<x=-8, y=-10, z=0>", "<x=5, y=5, z=10>", "<x=2, y=-7, z=3>", "<x=9, y=-8, z=-3>"], 100)
    1940
    '''
    axes = parse(lines)
    potentials = [0, 0, 0, 0]
    kinetics = [0, 0, 0, 0]
    for axis in axes:
        for i, (position, velocity) in enumerate(
                next(itertools.islice(simulate(axis), n - 1, n))):
            potentials[i] += abs(position)
            kinetics[i] += abs(velocity)
    return sum(map(operator.mul, potentials, kinetics))


def part2(lines):
    '''
    >>> part2(["<x=-1, y=0, z=2>", "<x=2, y=-10, z=-7>", "<x=4, y=-8, z=8>", "<x=3, y=5, z=-1>"])
    2772
    >>> part2(["<x=-8, y=-10, z=0>", "<x=5, y=5, z=10>", "<x=2, y=-7, z=3>", "<x=9, y=-8, z=-3>"])
    4686774924
    '''
    axes = parse(lines)
    result = 1
    for axis in axes:
        for i, state in enumerate(simulate(axis)):
            if axis == state:
                result = lcm(result, i + 1)
                break
    return result


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
