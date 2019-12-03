import fileinput


def walk(line):
    x, y = 0, 0
    for step in line.split(','):
        c, n = step[0], int(step[1:])
        for _ in range(n):
            yield (x, y)
            if c == 'U':
                y += 1
            elif c == 'L':
                x -= 1
            elif c == 'D':
                y -= 1
            elif c == 'R':
                x += 1
    yield (x, y)


def part1(lines):
    '''
    >>> part1(["R8,U5,L5,D3", "U7,R6,D4,L4"])
    6
    >>> part1(["R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"])
    159
    >>> part1(["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"])
    135
    '''
    result, field = None, {}
    for i, line in enumerate(lines):
        for x, y in walk(line):
            if (x or y) and field.setdefault((x, y), i) != i:
                manhattan = abs(x) + abs(y)
                if result is None or result > manhattan:
                    result = manhattan
    return result


def part2(lines):
    '''
    >>> part2(["R8,U5,L5,D3", "U7,R6,D4,L4"])
    30
    >>> part2(["R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"])
    610
    >>> part2(["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"])
    410
    '''
    result, field, distances = None, {}, {}
    for i, line in enumerate(lines):
        distance = 0
        for x, y in walk(line):
            if not x and not y:
                pass
            elif (x, y) not in field:
                field[(x, y)] = i
                distances[(x, y)] = distance
            elif field[(x, y)] != i:
                other = distances[(x, y)]
                if result is None or result > other + distance:
                    result = other + distance
                if other > distance:
                    field[(x, y)] = i
                    distances[(x, y)] = distance
            distance += 1
    return result


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
