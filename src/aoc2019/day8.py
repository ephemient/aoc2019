import fileinput


def part1(lines, /, width=25, height=6):
    line = lines[0].strip()
    chunk = min((line[i:i + width * height]
                 for i in range(0, len(line), width * height)),
                key=lambda s: s.count('0'))
    return chunk.count('1') * chunk.count('2')


def part2(lines, /, width=25, height=6):
    line = lines[0].strip()
    pixels = ''.join(
        ''.join(stack).lstrip('2')[:1] or '2'
        for stack in zip(*(line[i:i + width * height]
                           for i in range(0, len(line), width * height))))
    trans = str.maketrans('012', '\u2592\u2593\u2591')
    return '\n'.join(pixels[i * width:(i + 1) * width].translate(trans)
                     for i in range(height))


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
