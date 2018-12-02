import itertools
import collections

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
