import collections
import fileinput


def part1(lines):
    '''
    >>> part1("COM)B B)C C)D D)E E)F B)G G)H D)I E)J J)K K)L".split())
    42
    '''
    orbits = collections.defaultdict(set)
    for line in lines:
        x, y = line.split(')', maxsplit=1)
        orbits[x.strip()].add(y.strip())
    checksums = {}

    def checksum(x):
        if x not in checksums:
            checksums[x] = sum(checksum(y) + 1 for y in orbits.get(x, ()))
        return checksums[x]

    return sum(map(checksum, orbits))


def part2(lines):
    '''
    >>> part2("COM)B B)C C)D D)E E)F B)G G)H D)I E)J J)K K)L K)YOU I)SAN".split())
    4
    '''
    rorbits = {}
    for line in lines:
        x, y = line.split(')', maxsplit=1)
        rorbits[y.strip()] = x

    def parents(x):
        while x in rorbits:
            yield x
            x = rorbits[x]

    san, you = list(parents('SAN')), list(parents('YOU'))
    while san and you and san[-1] == you[-1]:
        del san[-1]
        del you[-1]
    return len(san) + len(you) - 2


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
