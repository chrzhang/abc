import itertools
import collections

# Helpers

def has_n_occ(n, s):
    c = collections.Counter(s)
    return any([v == n for v in dict(c).values()])

def differ_by_1(s1, s2):
    if len(s1) != len(s2):
        return False
    i = 0
    mismatch_found = False
    while i < len(s1):
        if s1[i] != s2[i]:
            if mismatch_found:
                return False
            else:
                mismatch_found = True
        i += 1
    return mismatch_found

def overlap(s1, s2):
    if len(s1) != len(s2):
        return None
    inds = filter(lambda i: (s1[i] == s2[i]), range(len(s1)))
    return ''.join([s1[i] for i in inds])

# Solutions

def day1a(input):
    return sum([int(x) for x in input.split(', ')])

def day1b(input):
    curr_freq = 0
    freqs_seen = set()
    for n in itertools.cycle(input.split(', ')):
        curr_freq += int(n)
        if curr_freq in freqs_seen:
            return curr_freq
        freqs_seen.add(curr_freq)


def day2a(input):
    count_2 = 0
    count_3 = 0
    for s in input:
        if has_n_occ(2, s):
            count_2 += 1
        if has_n_occ(3, s):
            count_3 += 1
    return count_2 * count_3

def day2b(input):
    for s1 in input:
        for s2 in input:
            if differ_by_1(s1, s2):
                return overlap(s1, s2)

