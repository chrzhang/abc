import itertools
from collections import Counter, defaultdict
import re

# Helpers


def has_n_occ(amount, text):
    ctr = Counter(text)
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


def mode_count(lst):
    ctr = Counter(lst)
    return ctr.most_common(1)[0]


def mode(lst):
    return mode_count(lst)[0]

# Solutions


def day1a(line):
    return sum([int(x) for x in line.split(', ')])


def day1b(line):
    curr_freq = 0
    freqs_seen = set([curr_freq])
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
    fabric = defaultdict(list)
    for line in lines:
        matched = re.match(
            r'^#(?P<fid>\d+) @ (?P<col>\d+),(?P<row>\d+): (?P<width>\d+)x(?P<height>\d+)$',
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


def day4h(lines):
    rebeginshift = r'^\[(?P<date>\d{4}-\d{2}-\d{2}) \d{2}:(?P<minute>\d{2})\] Guard #(?P<guard>\d+) begins shift$'  # noqa
    resleep = r'^\[(?P<date>\d{4}-\d{2}-\d{2}) \d{2}:(?P<minute>\d{2})\] falls asleep$'  # noqa
    rewake = r'^\[(?P<date>\d{4}-\d{2}-\d{2}) \d{2}:(?P<minute>\d{2})\] wakes up$'  # noqa
    curr_guard = None
    awake = 1
    asleep = 0
    curr_state = awake
    curr_sleep_interval_begin = None
    data = defaultdict(lambda: defaultdict(list))
    for line in lines:
        matched = re.match(rebeginshift, line)
        if matched:
            curr_guard = int(matched.group('guard'))
            continue
        matched = re.match(resleep, line)
        if matched:
            assert curr_state == awake
            curr_state = asleep
            curr_sleep_interval_begin = int(matched.group('minute'))
            continue
        matched = re.match(rewake, line)
        if matched:
            assert curr_state == asleep
            curr_state = awake
            for minute in range(curr_sleep_interval_begin,
                                int(matched.group('minute'))):
                data[curr_guard][matched.group('date')].append(minute)
            continue
    return data


def day4a(lines):
    data = day4h(lines)
    most_sleepy = max(data, key=lambda x: sum([len(m) for m in data[x].values()]))
    all_mins = [i for sublist in data[most_sleepy].values() for i in sublist]
    return most_sleepy * mode(all_mins)


def day4b(lines):
    data = day4h(lines)
    guards_to_most_sleepy_mins = {}
    for guard, dminutes in data.items():
        all_mins = [item for sublist in dminutes.values() for item in sublist]
        guards_to_most_sleepy_mins[guard] = mode_count(all_mins)
    chosen_guard = max(guards_to_most_sleepy_mins,
                       key=lambda x: guards_to_most_sleepy_mins[x][1])
    return chosen_guard * guards_to_most_sleepy_mins[chosen_guard][0]