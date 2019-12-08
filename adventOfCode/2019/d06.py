#!/usr/bin/python3

import re

with open("inputs/day6_input", "r") as f:
    read_lines = f.read().strip().split("\n")


def get_satellite_to_planet_map(lines):
    satellite_to_planet_map = {}
    for line in lines:
        m = re.match(r"^(?P<planet>\w+)\)(?P<satellite>\w+)$", line)
        planet = m.group("planet")
        satellite = m.group("satellite")
        satellite_to_planet_map[satellite] = planet
    return satellite_to_planet_map


def orbits(satellite, satellite_to_planet_map):
    if satellite in satellite_to_planet_map:
        return 1 + orbits(satellite_to_planet_map[satellite], satellite_to_planet_map)
    else:
        return 0


def solve(lines):
    satellite_to_planet_map = get_satellite_to_planet_map(lines)

    orbit_ctr = 0
    for satellite, planet in satellite_to_planet_map.items():
        orbit_ctr += orbits(satellite, satellite_to_planet_map)
    return orbit_ctr


assert 402879 == solve(read_lines)
