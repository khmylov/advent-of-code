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


# part1 - 245
print(len([x for x in read_input() if is_valid(x)]))
