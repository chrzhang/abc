import itertools
import collections
import re

# Helpers


def has_n_occ(amount, text):
    ctr = collections.Counter(text)
    return any([ct == amount for ct in dict(ctr).values()])


def differ_by_1(str1, str2):
    if len(str1) != len(str2):
        return False
    i = 0
    mismatch_found = False
    while i < len(str1):
        if str1[i] != str2[i]:
            if mismatch_found:
                return False
            else:
                mismatch_found = True
        i += 1
    return mismatch_found


def overlap(str1, str2):
    if len(str1) != len(str2):
        return None
    return ''.join([str1[i] for i in range(len(str1)) if str1[i] == str2[i]])

# Solutions


def day1a(line):
    return sum([int(x) for x in line.split(', ')])


def day1b(line):
    curr_freq = 0
    freqs_seen = set()
    for delta in itertools.cycle(line.split(', ')):
        curr_freq += int(delta)
        if curr_freq in freqs_seen:
            return curr_freq
        freqs_seen.add(curr_freq)
    return None


def day2a(lines):
    count_2 = 0
    count_3 = 0
    for line in lines:
        if has_n_occ(2, line):
            count_2 += 1
        if has_n_occ(3, line):
            count_3 += 1
    return count_2 * count_3


def day2b(lines):
    for line1 in lines:
        for line2 in lines:
            if differ_by_1(line1, line2):
                return overlap(line1, line2)
    return None


def day3h(lines):
    fabric = collections.defaultdict(list)
    for line in lines:
        matched = re.match(
            r'^#(?P<fid>\d+) @ (?P<col>\d+),(?P<row>\d+): (?P<width>\d+)x(?P<height>\d+)$',  # noqa
            line)
        if not matched:
            raise Exception('Line malformed: {0}'.format(line))
        fid = int(matched.group('fid'))
        col = int(matched.group('col'))
        row = int(matched.group('row'))
        width = int(matched.group('width'))
        height = int(matched.group('height'))
        coords = itertools.product(range(row, row + height),
                                   range(col, col + width))
        for coord in coords:
            fabric[coord].append(fid)
    return fabric


def day3a(lines):
    fabric = day3h(lines)
    return len([k for k, v in fabric.items() if len(v) > 1])


def day3b(lines):
    fabric = day3h(lines)
    all_ids = set([x for sublist in fabric.values() for x in sublist])
    for claims in fabric.values():
        if len(claims) > 1:
            for claim in claims:
                if claim in all_ids:
                    all_ids.remove(claim)
    if len(all_ids) != 1:
        raise Exception('No answer')
    return next(iter(all_ids))
