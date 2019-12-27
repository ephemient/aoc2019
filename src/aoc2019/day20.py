from collections import deque
import fileinput


def parse(lines):
    spaces = set((x, y) for y, line in enumerate(lines)
                 for x, c in enumerate(line) if c == '.')
    names = {}
    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            if not c.isalnum():
                continue
            if x + 1 < len(line) and line[x + 1].isalnum():
                s = c + line[x + 1]
                names[x, y] = s
                names[x + 1, y] = s
            if y + 1 < len(lines) and x < len(
                    lines[y]) and lines[y + 1][x].isalnum():
                s = c + lines[y + 1][x]
                names[x, y] = s
                names[x, y + 1] = s
    portals, reverse_portals = {}, {}
    for x, y in spaces:
        adjacent_names = [
            names[pos]
            for pos in ((x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y))
            if pos in names
        ]
        if adjacent_names:
            name = adjacent_names[0]
            portals[x, y] = name
            reverse_portals.setdefault(name, []).append((x, y))
    return spaces, portals, reverse_portals


SAMPLE_1 = """
         A           
         A           
  #######.#########  
  #######.........#  
  #######.#######.#  
  #######.#######.#  
  #######.#######.#  
  #####  B    ###.#  
BC...##  C    ###.#  
  ##.##       ###.#  
  ##...DE  F  ###.#  
  #####    G  ###.#  
  #########.#####.#  
DE..#######...###.#  
  #.#########.###.#  
FG..#########.....#  
  ###########.#####  
             Z       
             Z       
""".splitlines()
SAMPLE_2 = """
                   A               
                   A               
  #################.#############  
  #.#...#...................#.#.#  
  #.#.#.###.###.###.#########.#.#  
  #.#.#.......#...#.....#.#.#...#  
  #.#########.###.#####.#.#.###.#  
  #.............#.#.....#.......#  
  ###.###########.###.#####.#.#.#  
  #.....#        A   C    #.#.#.#  
  #######        S   P    #####.#  
  #.#...#                 #......VT
  #.#.#.#                 #.#####  
  #...#.#               YN....#.#  
  #.###.#                 #####.#  
DI....#.#                 #.....#  
  #####.#                 #.###.#  
ZZ......#               QG....#..AS
  ###.###                 #######  
JO..#.#.#                 #.....#  
  #.#.#.#                 ###.#.#  
  #...#..DI             BU....#..LF
  #####.#                 #.#####  
YN......#               VT..#....QG
  #.###.#                 #.###.#  
  #.#...#                 #.....#  
  ###.###    J L     J    #.#.###  
  #.....#    O F     P    #.#...#  
  #.###.#####.#.#####.#####.###.#  
  #...#.#.#...#.....#.....#.#...#  
  #.#####.###.###.#.#.#########.#  
  #...#.#.....#...#.#.#.#.....#.#  
  #.###.#####.###.###.#.#.#######  
  #.#.........#...#.............#  
  #########.###.###.#############  
           B   J   C               
           U   P   P               
""".splitlines()
SAMPLE_3 = """
             Z L X W       C                 
             Z P Q B       K                 
  ###########.#.#.#.#######.###############  
  #...#.......#.#.......#.#.......#.#.#...#  
  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  
  #.#...#.#.#...#.#.#...#...#...#.#.......#  
  #.###.#######.###.###.#.###.###.#.#######  
  #...#.......#.#...#...#.............#...#  
  #.#########.#######.#.#######.#######.###  
  #...#.#    F       R I       Z    #.#.#.#  
  #.###.#    D       E C       H    #.#.#.#  
  #.#...#                           #...#.#  
  #.###.#                           #.###.#  
  #.#....OA                       WB..#.#..ZH
  #.###.#                           #.#.#.#  
CJ......#                           #.....#  
  #######                           #######  
  #.#....CK                         #......IC
  #.###.#                           #.###.#  
  #.....#                           #...#.#  
  ###.###                           #.#.#.#  
XF....#.#                         RF..#.#.#  
  #####.#                           #######  
  #......CJ                       NM..#...#  
  ###.#.#                           #.###.#  
RE....#.#                           #......RF
  ###.###        X   X       L      #.#.#.#  
  #.....#        F   Q       P      #.#.#.#  
  ###.###########.###.#######.#########.###  
  #.....#...#.....#.......#...#.....#.#...#  
  #####.#.###.#######.#######.###.###.#.#.#  
  #.......#.......#.#.#.#.#...#...#...#.#.#  
  #####.###.#####.#.#.#.#.###.###.#.###.###  
  #.......#.....#.#...#...............#...#  
  #############.#.#.###.###################  
               A O F   N                     
               A A D   M                     
""".splitlines()


def part1(lines):
    '''
    >>> part1(SAMPLE_1)
    23
    >>> part1(SAMPLE_2)
    58
    '''
    spaces, portals, reverse_portals = parse(lines)
    start, = reverse_portals['AA']
    queue, seen = deque(((0, *start), )), {start}
    while queue:
        d, x, y = queue.popleft()
        if portals.get((x, y)) == 'ZZ':
            return d
        for pos in ((x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)):
            if pos in spaces and pos not in seen:
                queue.append((d + 1, *pos))
                seen.add(pos)
        for pos in reverse_portals.get(portals.get((x, y)), ()):
            if pos != (x, y) and pos not in seen:
                queue.append((d + 1, *pos))
                seen.add(pos)


def part2(lines):
    '''
    >>> part2(SAMPLE_1)
    26
    >>> part2(SAMPLE_2)
    >>> part2(SAMPLE_3)
    396
    '''
    spaces, portals, reverse_portals = parse(lines)
    extreme_x = min(x for x, _ in spaces), max(x for x, _ in spaces)
    extreme_y = min(y for _, y in spaces), max(y for _, y in spaces)
    start, = reverse_portals['AA']
    queue, seen = deque(((0, *start, 0), )), {(*start, 0)}
    while queue:
        d, x, y, z = queue.popleft()
        if z == 0 and portals.get((x, y)) == 'ZZ':
            return d
        if z > len(reverse_portals):
            continue
        for pos in ((x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)):
            if pos in spaces and (*pos, z) not in seen:
                queue.append((d + 1, *pos, z))
                seen.add((*pos, z))
        for x2, y2 in reverse_portals.get(portals.get((x, y)), ()):
            if x2 == x and y2 == y:
                continue
            if x in extreme_x or y in extreme_y:
                if z > 0 and (x2, y2, z - 1) not in seen:
                    queue.append((d + 1, x2, y2, z - 1))
                    seen.add((x2, y2, z - 1))
            else:
                queue.append((d + 1, x2, y2, z + 1))
                seen.add((x2, y2, z + 1))


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
