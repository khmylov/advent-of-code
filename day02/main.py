import re
from typing import Tuple

policy_regexp = re.compile(r'^(\d+)-(\d+) (.): (.+)$')

Policy = Tuple[int, int, str, str]


def parse(input: str) -> Policy:
    m = policy_regexp.match(input)
    return int(m.group(1)), int(m.group(2)), m.group(3), m.group(4)


def matches_part1(policy: Policy) -> bool:
    count = len([c for c in policy[3] if c == policy[2]])
    return policy[0] <= count <= policy[1]


def matches_part2(policy: Policy) -> bool:
    char, password = policy[2], policy[3]
    return (password[policy[0] - 1] == char) ^ (password[policy[1] - 1] == char)


policies = list(map(parse, open('input.txt').readlines()))

# part1
print(len(list(filter(matches_part1, policies))))

# part2
print(len(list(filter(matches_part2, policies))))
