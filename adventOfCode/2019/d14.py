#!/usr/bin/env python3
import re
from collections import defaultdict
from math import ceil


def make_recipe_book(lines):
    recipe_book = {}
    for line in lines:
        m = re.match(r"^(?P<in>.*) => (?P<out_ct>[0-9]+) (?P<out_t>[A-Z]+)$", line)
        inputs = []
        for i in m.group("in").split(", "):
            input_ct, input_v = i.split(" ")
            inputs.append((input_v, int(input_ct)))
        recipe_book[m.group("out_t")] = (int(m.group("out_ct")), inputs)
    return recipe_book


def get_batch_size(chem_t, recipe_book):
    return recipe_book[chem_t][0]


def get_reagents(chem_t, recipe_book):
    return recipe_book[chem_t][1]


def get_ore_count(chem_t, chem_ct, leftovers, recipe_book):
    if chem_t == "ORE":
        return chem_ct
    if leftovers[chem_t] >= chem_ct:
        leftovers[chem_t] -= chem_ct
        return 0
    chem_ct -= leftovers[chem_t]
    leftovers[chem_t] = 0
    ore_ct = 0
    batch_size = get_batch_size(chem_t, recipe_book)
    batch_ct = ceil(chem_ct / batch_size)
    reagents = get_reagents(chem_t, recipe_book)
    for reagent_t, reagent_ct in reagents:
        ore_ct += get_ore_count(
            reagent_t, reagent_ct * batch_ct, leftovers, recipe_book
        )
    leftovers[chem_t] += batch_ct * batch_size - chem_ct
    return ore_ct


def part_1(filename):
    with open(filename, "r") as f:
        read_lines = f.read().strip().split("\n")

    recipe_book = make_recipe_book(read_lines)
    leftovers = defaultdict(int)
    return get_ore_count("FUEL", 1, leftovers, recipe_book)


def part_2(filename):
    with open(filename, "r") as f:
        read_lines = f.read().strip().split("\n")

    recipe_book = make_recipe_book(read_lines)
    lo = 1
    hi = 10 ** 12
    highest_amount_of_fuel = None
    while lo < hi:
        mid = (hi + lo) // 2
        leftovers = defaultdict(int)
        ore_ct = get_ore_count("FUEL", mid, leftovers, recipe_book)
        highest_amount_of_fuel = mid
        if ore_ct > 10 ** 12:
            hi = mid - 1
            highest_amount_of_fuel -= 1
        else:
            lo = mid + 1
            highest_amount_of_fuel += 1
    return highest_amount_of_fuel


def run_tests():
    assert part_1("inputs/day14_input_ex1") == 31
    assert part_1("inputs/day14_input_ex2") == 165
    assert part_1("inputs/day14_input_ex3") == 13312
    assert part_1("inputs/day14_input_ex4") == 180697
    assert part_1("inputs/day14_input_ex5") == 2210736
    assert part_2("inputs/day14_input_ex3") == 82892753
    assert part_2("inputs/day14_input_ex4") == 5586022
    assert part_2("inputs/day14_input_ex5") == 460664


if __name__ == "__main__":
    run_tests()
    assert part_1("inputs/day14_input") == 1046184
    assert part_2("inputs/day14_input") == 1639374
