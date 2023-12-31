import pathlib
from pydoc import doc
import re
from collections import namedtuple

Entry = namedtuple("Entry", ["value", "column_begin", "column_end"])

input_file_name = pathlib.Path(__file__).parent.joinpath("input.txt")

with open(input_file_name, "r") as file:
    input_lines = [line.strip() for line in file.readlines()]
    rows = [
        [
            Entry(
                value=int(match.group()),
                column_begin=match.span()[0],
                column_end=match.span()[1],
            )
            for match in re.finditer(r"(\d+)", line)
        ]
        for line in input_lines
    ]

symbols = [
    (x, y, char)
    for (y, line) in enumerate(input_lines)
    for (x, char) in enumerate(line)
    if not (char.isdigit() or char == ".")
]


def is_near_x(x, entry):
    return (
        entry.column_end == x
        or entry.column_begin == x + 1
        or (entry.column_begin <= x and entry.column_end >= x)
    )


def find_adjacent(x, y):
    return (
        entry
        for row_index in range(max(0, y - 1), min(y + 2, len(rows)))
        for entry in rows[row_index]
        if is_near_x(x, entry)
    )


def part1():
    return sum(item.value for (x, y, _) in symbols for item in find_adjacent(x, y))


def part2():
    return sum(
        adjacent[0].value * adjacent[1].value
        for (x, y, c) in symbols
        if c == "*" and len(adjacent := list(find_adjacent(x, y))) == 2
    )


if __name__ == "__main__":
    print("part1: ", part1())
    print("part2: ", part2())
