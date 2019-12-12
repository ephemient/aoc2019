from dataclasses import dataclass
import fileinput
import functools
import itertools

import re

PATTERN = re.compile(r'-?\d+')


def lcm(x, y):
    a, b = x, y
    while a:
        a, b = b % a, a
    return x // b * y


def sign(n):
    return -1 if n < 0 else 1 if n > 0 else 0


def vector(line):
    return Vector(*(int(s.group()) for s in re.finditer(PATTERN, line)))


@dataclass
class Vector:
    x: int
    y: int
    z: int

    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y, self.z + other.z)

    def sign_to(self, other):
        return Vector(sign(other.x - self.x), sign(other.y - self.y),
                      sign(other.z - self.z))

    def abs_sum(self):
        return abs(self.x) + abs(self.y) + abs(self.z)


def simulate(points):
    velocities = [Vector(0, 0, 0) for _ in points]
    n = 0
    while True:
        for i, p1 in enumerate(points):
            velocities[i] = functools.reduce(
                lambda acc, p2: acc + p1.sign_to(p2), points, velocities[i])
        for i, velocity in enumerate(velocities):
            points[i] = points[i] + velocity
        yield list(zip(points, velocities))


def part1(lines, n=1000):
    '''
    >>> part1(["<x=-1, y=0, z=2>", "<x=2, y=-10, z=-7>", "<x=4, y=-8, z=8>", "<x=3, y=5, z=-1>"], 10)
    179
    >>> part1(["<x=-8, y=-10, z=0>", "<x=5, y=5, z=10>", "<x=2, y=-7, z=3>", "<x=9, y=-8, z=-3>"], 100)
    1940
    '''
    points = list(map(vector, lines))
    return sum(p.abs_sum() * v.abs_sum()
               for p, v in next(itertools.islice(simulate(points), n - 1, n)))


def part2(lines):
    '''
    >>> part2(["<x=-1, y=0, z=2>", "<x=2, y=-10, z=-7>", "<x=4, y=-8, z=8>", "<x=3, y=5, z=-1>"])
    2772
    >>> part2(["<x=-8, y=-10, z=0>", "<x=5, y=5, z=10>", "<x=2, y=-7, z=3>", "<x=9, y=-8, z=-3>"])
    4686774924
    '''
    points = list(map(vector, lines))
    seenX, seenY, seenZ = set(), set(), set()
    for state in simulate(points):
        stateX = tuple((p.x, v.x) for p, v in state)
        stateY = tuple((p.y, v.y) for p, v in state)
        stateZ = tuple((p.z, v.z) for p, v in state)
        if stateX in seenX and stateY in seenY and stateZ in seenZ:
            break
        seenX.add(stateX)
        seenY.add(stateY)
        seenZ.add(stateZ)
    return lcm(len(seenX), lcm(len(seenY), len(seenZ)))


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
