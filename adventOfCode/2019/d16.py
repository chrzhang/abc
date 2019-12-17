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


def part_1(line, phase_ct):
    digits = [int(d) for d in line]
    for _ in range(phase_ct):
        digits = fft(digits)
    return "".join(str(d) for d in digits)


def run_tests():
    assert part_1("12345678", 1) == "48226158"
    assert part_1("12345678", 2) == "34040438"
    assert part_1("12345678", 3) == "03415518"
    assert part_1("12345678", 4) == "01029498"
    assert part_1("80871224585914546619083218645595", 100)[:8] == "24176176"
    assert part_1("19617804207202209144916044189917", 100)[:8] == "73745418"
    assert part_1("69317163492948606335995924319873", 100)[:8] == "52432133"


if __name__ == "__main__":
    run_tests()
    with open("inputs/day16_input", "r") as f:
        (line,) = f.read().strip().split("\n")
    assert "69549155" == part_1(line, 100)[:8]
