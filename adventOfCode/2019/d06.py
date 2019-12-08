#!/usr/bin/python3

import re
from collections import defaultdict

with open("inputs/day6_input", "r") as f:
    read_lines = f.read().strip().split("\n")


def parse_planet_satellite(line):
    m = re.match(r"^(?P<planet>\w+)\)(?P<satellite>\w+)$", line)
    planet = m.group("planet")
    satellite = m.group("satellite")
    return planet, satellite

def get_satellite_to_planet_map(lines):
    satellite_to_planet_map = {}
    for line in lines:
        planet, satellite = parse_planet_satellite(line)
        satellite_to_planet_map[satellite] = planet
    return satellite_to_planet_map


def orbits(satellite, satellite_to_planet_map):
    if satellite in satellite_to_planet_map:
        return 1 + orbits(satellite_to_planet_map[satellite], satellite_to_planet_map)
    else:
        return 0


def total_orbits(lines):
    satellite_to_planet_map = get_satellite_to_planet_map(lines)

    orbit_ctr = 0
    for satellite, planet in satellite_to_planet_map.items():
        orbit_ctr += orbits(satellite, satellite_to_planet_map)
    return orbit_ctr


assert 402879 == total_orbits(read_lines)

def get_system_map(lines):
    satellite_to_planet_map = {}
    planet_to_satellite_map = defaultdict(list)
    for line in lines:
        planet, satellite = parse_planet_satellite(line)
        satellite_to_planet_map[satellite] = planet
        planet_to_satellite_map[planet].append(satellite)
    return satellite_to_planet_map, planet_to_satellite_map

def neighbors(planet, satellite_to_planet_map, planet_to_satellite_map):
    things_orbited_by_planet = [satellite_to_planet_map.get(planet)]
    if things_orbited_by_planet == [None]:
        things_orbited_by_planet = []
    return things_orbited_by_planet + planet_to_satellite_map[planet]

class CouldNotFindPathError(Exception):
    pass

def min_orbital_transfers(from_planet, to_planet):
    satellite_to_planet_map, planet_to_satellite_map = get_system_map(read_lines)
    visited = set()
    curr_frontier = set({'YOU'})
    curr_level = 0
    while True:
        if not curr_frontier:
            raise CouldNotFindPathError
        if 'SAN' in curr_frontier:
            return curr_level
        next_frontier = set()
        for p in curr_frontier:
            visited.add(p)
        for p in curr_frontier:
            ns = neighbors(p, satellite_to_planet_map, planet_to_satellite_map)
            unvisited_ns = {n for n in ns if n not in visited}
            next_frontier |= unvisited_ns
        curr_frontier = next_frontier
        curr_level += 1

assert 484 == min_orbital_transfers('YOU', 'SAN') - 2
