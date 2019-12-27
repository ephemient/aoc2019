import dataclasses
import fileinput
import functools
from typing import Union


@dataclasses.dataclass(frozen=True)
class Shuffle:
    base: int
    m: int
    c: int

    @classmethod
    def identity(cls, base: int) -> 'Shuffle':
        return cls(base=base, m=1, c=0)

    def __invert__(self) -> 'Shuffle':
        return dataclasses.replace(self,
                                   m=(-self.m) % self.base,
                                   c=(-1 - self.c) % self.base)

    def __add__(self, x: int) -> 'Shuffle':
        return dataclasses.replace(self, c=(self.c + x) % self.base)

    def __mul__(self, x: Union[int, 'Shuffle']) -> 'Shuffle':
        if isinstance(x, Shuffle):
            assert self.base == x.base
            return dataclasses.replace(self,
                                       m=(self.m * x.m) % self.base,
                                       c=(self.c * x.m + x.c) % self.base)
        return dataclasses.replace(self,
                                   m=(self.m * x) % self.base,
                                   c=(self.c * x) % self.base)

    def __pow__(self, x: int) -> 'Shuffle':
        assert x >= 0
        if not x:
            return dataclasses.replace(self, m=1, c=0)
        if x % 2:
            return self * (self * self)**(x // 2)
        return (self * self)**(x / 2)

    def __call__(self, x: int) -> int:
        return (self.m * x + self.c) % self.base

    def __getitem__(self, x: int) -> int:
        n = pow(self.m, -1, self.base)
        return (n * (x - self.c) % self.base)


def apply_shuffle(shuffle, line):
    words = line.split()
    if words[2:3] == ['new']:
        return ~shuffle
    if words[0:1] == ['cut']:
        return shuffle + -int(words[1])
    if words[2:3] == ['increment']:
        return shuffle * int(words[3])


def part1(lines):
    return functools.reduce(apply_shuffle, lines,
                            Shuffle.identity(10007))(2019)


def part2(lines):
    return (functools.reduce(
        apply_shuffle, lines,
        Shuffle.identity(119315717514047))**101741582076661)[2020]


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
