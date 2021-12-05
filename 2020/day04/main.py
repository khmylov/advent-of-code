import re


def read_input():
    result, current_item = [], {}
    for line in open('input.txt').read().splitlines():
        if line == '':
            result.append(current_item)
            current_item = {}
        else:
            for data_entry in line.split(' '):
                data = data_entry.split(':')
                current_item[data[0]] = data[1]
    if len(current_item) > 0:
        result.append(current_item)
    return result


def is_valid(item):
    return ('byr' in item
            and 'iyr' in item
            and 'eyr' in item
            and 'hgt' in item
            and 'hcl' in item
            and 'ecl' in item
            and 'pid' in item)


def is_valid2(item):
    if not is_valid(item):
        return False

    byr = int(item['byr'])
    if byr < 1920 or byr > 2002:
        return False
    iyr = int(item['iyr'])
    if iyr < 2010 or iyr > 2020:
        return False
    eyr = int(item['eyr'])
    if eyr < 2020 or eyr > 2030:
        return False
    hgt_text = item['hgt']
    if hgt_text.endswith("cm"):
        hgt = int(hgt_text[:-2])
        if hgt < 150 or hgt > 193:
            return False
    elif hgt_text.endswith("in"):
        hgt = int(hgt_text[:-2])
        if hgt < 59 or hgt > 76:
            return False
    else:
        return False
    if re.compile(r'^#[0-9a-f]{6}$').match(item['hcl']) is None:
        return False
    if re.compile(r'^(amb|blu|brn|gry|grn|hzl|oth)$').match(item['ecl']) is None:
        return False
    if re.compile(r'^[0-9]{9}$').match(item['pid']) is None:
        return False
    return True


# part1 - 245
print(len([x for x in read_input() if is_valid(x)]))

# part2 - 133
print(len([x for x in read_input() if is_valid2(x)]))
