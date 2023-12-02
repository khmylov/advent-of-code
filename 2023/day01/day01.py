import os

input_file_name = os.path.join(os.path.dirname(os.path.abspath(__file__)), "input.txt")


def first_numeric_char(line):
    for i, char in enumerate(line):
        if char.isdigit():
            return i, int(char)


def last_numeric_char(line):
    for i, char in reversed(list(enumerate(line))):
        if char.isdigit():
            return i, int(char)


def part1():
    with open(input_file_name) as input_file:
        result = 0
        for line in input_file:
            first_char = first_numeric_char(line)
            last_char = last_numeric_char(line)
            if first_char is not None and last_char is not None:
                result += first_char[1] * 10 + last_char[1]
    return result


def part2():
    words = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

    def find_first(line):
        word_at = len(line)
        word_value = None
        for word_index, word in enumerate(words):
            index = line.find(word)
            if index >= 0 and index < word_at:
                word_at = index
                word_value = word_index + 1

        first_char = first_numeric_char(line)
        if first_char is not None and first_char[0] < word_at:
            return first_char[1]
        return word_value

    def find_last(line):
        word_at = -1
        word_value = None
        for word_index, word in enumerate(words):
            index = line.rfind(word)
            if index >= 0 and index > word_at:
                word_at = index
                word_value = word_index + 1

        last_char = last_numeric_char(line)
        if last_char is not None and last_char[0] > word_at:
            return last_char[1]
        return word_value

    with open(input_file_name) as input_file:
        result = 0
        for line in input_file:
            first, last = find_first(line), find_last(line)
            print(first, last)
            if first is not None and last is not None:
                result += first * 10 + last
        return result


if __name__ == "__main__":
    print("part1:", part1())  # 54990
    print("part2:", part2())  # 54473
