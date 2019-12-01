#!/usr/bin/python3

with open('inputs/day1_input', 'r') as f:
    lines = f.read().strip().split('\n')

masses = [int(line) for line in lines]
fuel_requirements = [mass // 3 - 2 for mass in masses]
assert 3394106 == sum(fuel_requirements)

sum_of_fuel_fuel_requirements = 0
for mass in masses:
    fuel_amount = mass // 3 - 2
    while fuel_amount > 0:
        sum_of_fuel_fuel_requirements += fuel_amount
        fuel_amount = fuel_amount // 3 - 2
assert 5088280 == sum_of_fuel_fuel_requirements
