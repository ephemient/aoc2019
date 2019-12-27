from collections import defaultdict
import fileinput
import heapq


def part1(lines):
    '''
    >>> part1("######### #b.A.@.a# #########".split())
    8
    >>> part1("######################## #f.D.E.e.C.b.A.@.a.B.c.# ######################.# #d.....................# ########################".split())
    86
    >>> part1("######################## #...............b.C.D.f# #.###################### #.....@.a.B.c.d.A.e.F.g# ########################".split())
    132
    >>> part1("################# #i.G..c...e..H.p# ########.######## #j.A..b...f..D.o# ########@######## #k.E..a...g..B.n# ########.######## #l.F..d...h..C.m# #################".split())
    136
    >>> part1("######################## #@..............ac.GI.b# ###d#e#f################ ###A#B#C################ ###g#h#i################ ########################".split())
    81
    '''
    all_keys = {(x, y): c
                for y, line in enumerate(lines) for x, c in enumerate(line)
                if c.islower()}
    starts = dict(all_keys)
    bots = []
    for bot, pos in enumerate((x, y) for y, line in enumerate(lines)
                              for x, c in enumerate(line) if c == '@'):
        starts[pos] = str(bot)
        bots.append(str(bot))
    all_paths = {}
    for pos, item in starts.items():
        queue, seen, item_paths = [(0, pos, frozenset())], {pos}, {}
        while queue:
            d, pos, doors = heapq.heappop(queue)
            x, y = pos
            c = lines[y][x]
            if c != item and c.islower():
                item_paths[c] = (d, doors)
                continue
            elif c.isupper():
                doors = doors | {c.lower()}
            for x, y in ((x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)):
                if (x, y) in seen or not (0 <= y < len(lines)):
                    continue
                line = lines[y]
                if not (0 <= x < len(line)):
                    continue
                c = line[x]
                if c not in '.@' and not c.isalpha():
                    continue
                heapq.heappush(queue, (d + 1, (x, y), doors))
                seen.add((x, y))
        all_paths[item] = item_paths
    queue, seen = [(0, tuple(bots), frozenset())], set()
    while queue:
        d, items, keys = heapq.heappop(queue)
        if (items, keys) in seen:
            continue
        if len(keys) == len(all_keys):
            return d
        seen.add((items, keys))
        for i, item in enumerate(items):
            for item2, value in all_paths[item].items():
                d2, doors = value
                if doors - keys:
                    continue
                items2 = list(items)
                items2[i] = item2
                heapq.heappush(queue, (d + d2, tuple(items2), keys | {item2}))


def part2(lines):
    '''
    >>> part2("####### #a.#Cd# ##...## ##.@.## ##...## #cB#Ab# #######".split())
    8
    >>> part2("############### #d.ABC.#.....a# ######...###### ######.@.###### ######...###### #b.....#.....c# ###############".split())
    24
    >>> part2("############# #DcBa.#.GhKl# #.###...#I### #e#d#.@.#j#k# ###C#...###J# #fEbA.#.FgHi# #############".split())
    32
    >>> part2("############# #g#f.D#..h#l# #F###e#E###.# #dCba...BcIJ# #####.@.##### #nK.L...G...# #M###N#H###.# #o#m..#i#jk.# #############".split())
    72
    '''
    x, y = next((x, y) for y, line in enumerate(lines)
                for x, c in enumerate(line) if c == '@')
    return part1(lines[:y - 1] + [
        lines[y - 1][:x - 1] + '@#@' + lines[y - 1][x + 2:], lines[y][:x - 1] +
        '###' + lines[y][x + 2:], lines[y + 1][:x - 1] + '@#@' +
        lines[y + 1][x + 2:]
    ] + lines[y + 2:])


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
