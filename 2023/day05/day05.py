import pathlib
from collections import namedtuple

MapEntry = namedtuple("MapEntry", ["dest_start", "source_start", "length"])

with open(pathlib.Path(__file__).parent.joinpath("input.txt")) as input_file:
    input_sections = input_file.read().split("\n\n")
    input_seeds = [int(t) for t in input_sections[0].split(": ")[1].split()]
    mappings = [
        [MapEntry(*map(int, line.split())) for line in section.splitlines()[1:]]
        for section in input_sections[1:]
    ]


def part1():
    def loop():
        for current in input_seeds:
            for mapping in mappings:
                for dest_start, source_start, length in mapping:
                    if (diff := current - source_start) >= 0 and diff < length:
                        current = diff + dest_start
                        break
            yield current

    return min(loop())


def part2():
    input_seed_ranges = list(zip(input_seeds[::2], input_seeds[1::2]))

    def loop():
        for seed_start, seed_length in input_seed_ranges:
            for current in range(seed_start, seed_start + seed_length + 1):
                for mapping in mappings:
                    for dest_start, source_start, length in mapping:
                        if (diff := current - source_start) >= 0 and diff < length:
                            current = diff + dest_start
                            break
                yield current

    return min(loop())


if __name__ == "__main__":
    print("part1:", part1())  # 484023871
    # print("part2:", part2()) # too slow
