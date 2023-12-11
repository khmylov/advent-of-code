import pathlib

input_file_name = pathlib.Path(__file__).parent.joinpath("input.txt")


def get_input():
    def parse_cube_spec(cube_spec: str):
        [num, color] = cube_spec.split(" ", 1)
        return int(num), color

    with open(input_file_name) as file:
        for line in file:
            _, spec = line.strip().split(": ", 1)
            yield [
                [parse_cube_spec(cube) for cube in cube_spec.split(", ")]
                for cube_spec in spec.split("; ")
            ]


def of_color(cube_set, color):
    return next((cube[0] for cube in cube_set if cube[1] == color), 0)


def part1():
    return sum(
        game_index
        for game_index, game_spec in enumerate(get_input(), start=1)
        if all(
            of_color(cube_set, "red") <= 12
            and of_color(cube_set, "green") <= 13
            and of_color(cube_set, "blue") <= 14
            for cube_set in game_spec
        )
    )


def part2():
    def max_by_color(game_spec, color):
        return max(of_color(cube_set, color) for cube_set in game_spec)

    return sum(
        max_by_color(game_spec, "red")
        * max_by_color(game_spec, "green")
        * max_by_color(game_spec, "blue")
        for game_spec in get_input()
    )


if __name__ == "__main__":
    print("part1:", part1())  # 2105
    print("part2:", part2())  # 72422
