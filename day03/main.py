from math import prod

in_data = open('input.txt').readlines()


def check(dx: int, dy: int) -> int:
    x, y = 0, 0
    tree_count = 0
    while y < len(in_data):
        row = in_data[y]
        row_len = len(row) - 1
        if row[x % row_len] == '#':
            tree_count = tree_count + 1

        x, y = x + dx, y + dy
    return tree_count


# part1
print(check(3, 1))

# part2
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
print(prod(check(x, y) for (x, y) in slopes))
