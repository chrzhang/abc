#!/usr/bin/env python3


def get_digits(n):
    return [int(c) for c in str(n)]


def has_2_same_adj_digits(digits):
    for i in range(5):
        if digits[i] == digits[i + 1]:
            return True
    return False


def never_decreasing_digits(digits):
    for i in range(5):
        if digits[i + 1] < digits[i]:
            return False
    return True


def is_part_1_candidate(digits):
    if len(digits) != 6:
        return False
    if not has_2_same_adj_digits(digits):
        return False
    if not never_decreasing_digits(digits):
        return False
    return True


def digits_only_occur_at(m, n, digits):
    digit = digits[m]
    for i in range(6):
        if i in (m, n):
            continue
        if digits[i] == digit:
            return False
    return True


def has_2_same_adj_digits_unique(digits):
    for i in range(5):
        if digits[i] == digits[i + 1]:
            if digits_only_occur_at(i, i + 1, digits):
                return True
    return False


def is_part_2_candidate(digits):
    return has_2_same_adj_digits_unique(digits)


assert is_part_1_candidate(get_digits(111111))
assert not is_part_1_candidate(get_digits(223450))
assert not is_part_1_candidate(get_digits(123789))

assert is_part_2_candidate(get_digits(112233))
assert not is_part_2_candidate(get_digits(123444))
assert is_part_2_candidate(get_digits(111122))


def get_password_candidates(lower, upper):
    part_1_ctr = 0
    part_2_ctr = 0
    for candidate in range(lower, upper + 1):
        digits = get_digits(candidate)
        if is_part_1_candidate(digits):
            part_1_ctr += 1
            if is_part_2_candidate(digits):
                part_2_ctr += 1
    return part_1_ctr, part_2_ctr


part_1_candidate_count, part_2_candidate_count = get_password_candidates(172851, 675869)
assert 1660 == part_1_candidate_count
assert 1135 == part_2_candidate_count
