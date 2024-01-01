import pathlib
from queue import Queue


def matching_numbers_count(card):
    return len(card[0] & card[1])


with open(pathlib.Path(__file__).parent.joinpath("input.txt"), "r") as input_file:
    cards = [
        matching_numbers_count(
            [
                set(int(num_text) for num_text in entry.split())
                for entry in line.strip().split(": ")[1].split(" | ")
            ]
        )
        for line in input_file.readlines()
    ]


def part1():
    return sum(2 ** (card - 1) for card in cards if card > 0)


def part2():
    q = Queue()
    for index, _ in enumerate(cards):
        q.put(index)

    result = 0

    while not q.empty():
        result += 1
        card_index = q.get()
        for win_index in range(card_index + 1, card_index + cards[card_index] + 1):
            q.put(win_index)

    return result


if __name__ == "__main__":
    print("part1", part1())  # 22674
    print("part2", part2())  # 5747443
