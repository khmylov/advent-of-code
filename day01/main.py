numbers = [int(line) for line in open('input.txt').read().splitlines()]

# part1
print(next(x * y for x in numbers for y in numbers if x + y == 2020))

# part2
print(next(x * y * z for x in numbers for y in numbers for z in numbers if x + y + z == 2020))
