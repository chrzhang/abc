#!/usr/bin/env python3

from itertools import cycle


def flatten(f):
    return [l for t in f for l in t]


def get_pattern_for(idx):
    base_pattern = [0, 1, 0, -1]
    repeated_pattern = flatten([(1 + idx) * [e] for e in base_pattern])
    return cycle(repeated_pattern)


def fft(digits):
    new_digits = []
    for idx, _ in enumerate(digits):
        pattern = get_pattern_for(idx)
        next(pattern)  # Throw away first digit
        total = 0
        for d in digits:
            total += d * next(pattern)
        new_digits.append(abs(total) % 10)
    return new_digits


def fft_second_half(second_half):
    new_half = []
    total_sum = sum(second_half)
    for idx, _ in enumerate(second_half):
        new_half.append(total_sum % 10)
        total_sum -= second_half[idx]
    return new_half


def part_1(line, phase_ct):
    digits = [int(d) for d in line]
    for _ in range(phase_ct):
        digits = fft(digits)
    return "".join([str(d) for d in digits][:8])


def get_offset(line):
    return int(line[:7])


def get_message_at_offset(line, offset):
    return line[offset : offset + 8]


def part_2(line, phase_ct):
    offset = get_offset(line)
    digits = [int(d) for d in line * 10000]
    second_half = digits[len(digits) // 2 :]
    for _ in range(phase_ct):
        second_half = fft_second_half(second_half)
    offset_in_second_half = offset - len(digits) // 2
    second_half_str = "".join([str(d) for d in second_half])
    return get_message_at_offset(second_half_str, offset_in_second_half)


def run_tests():
    assert part_1("12345678", 1) == "48226158"
    assert part_1("12345678", 2) == "34040438"
    assert part_1("12345678", 3) == "03415518"
    assert part_1("12345678", 4) == "01029498"
    assert part_1("80871224585914546619083218645595", 100) == "24176176"
    assert part_1("19617804207202209144916044189917", 100) == "73745418"
    assert part_1("69317163492948606335995924319873", 100) == "52432133"
    assert get_offset("123456789") == 1234567
    assert get_message_at_offset("98765432109876543210", 7) == "21098765"
    assert [6, 1, 5, 8] == fft_second_half([5, 6, 7, 8])
    assert part_2("03036732577212944063491565474664", 100) == "84462026"
    assert part_2("02935109699940807407585447034323", 100) == "78725270"
    assert part_2("03081770884921959731165446850517", 100) == "53553731"


if __name__ == "__main__":
    run_tests()
    with open("inputs/day16_input", "r") as f:
        (line,) = f.read().strip().split("\n")
    assert "69549155" == part_1(line, 100)
    assert "83253465" == part_2(line, 100)
