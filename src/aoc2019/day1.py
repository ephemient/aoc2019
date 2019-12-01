import fileinput


def fuel(weight):
    return weight // 3 - 2


def fuels(weight):
    while (weight := fuel(weight)) > 0:
        yield weight


def part1(lines):
    '''
    >>> part1(['12'])
    2
    >>> part1(['14'])
    2
    >>> part1(['1969'])
    654
    >>> part1(['100756'])
    33583
    '''
    return sum(fuel(int(line)) for line in lines)


def part2(lines):
    '''
    >>> part2(['14'])
    2
    >>> part2(['1969'])
    966
    >>> part2(['100756'])
    50346
    '''
    return sum(weight for line in lines for weight in fuels(int(line)))


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
