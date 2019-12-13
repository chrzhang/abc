#!/usr/bin/env python3
import re
from math import gcd


def lcm(values):
    if len(values) > 2:
        return lcm([values[0], lcm(values[1:])])
    elif len(values) == 2:
        a = int(values[0])
        b = int(values[1])
        return a * b / gcd(a, b)
    else:
        raise RuntimeError


class Moon:
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z
        self.dx, self.dy, self.dz = (0, 0, 0)

    def apply_gravity(self, other_moon):
        for coord in "xyz":
            curr_pos = getattr(self, coord)
            curr_vel = getattr(self, f"d{coord}")
            if getattr(other_moon, coord) > curr_pos:
                setattr(self, f"d{coord}", curr_vel + 1)
            elif getattr(other_moon, coord) < curr_pos:
                setattr(self, f"d{coord}", curr_vel - 1)

    def apply_velocity(self):
        self.x += self.dx
        self.y += self.dy
        self.z += self.dz

    @property
    def potential_energy(self):
        return sum([abs(a) for a in [self.x, self.y, self.z]])

    @property
    def kinetic_energy(self):
        return sum([abs(a) for a in [self.dx, self.dy, self.dz]])

    def __str__(self):
        return f"pos=<x={self.x}, y={self.y}, z={self.z}>, vel=<x={self.dx}, y={self.dy}, z={self.dz}>"


def get_moons(lines):
    moons = []
    for line in lines:
        m = re.match(r"^<x=(?P<x>.+), y=(?P<y>.+), z=(?P<z>.+)>$", line)
        moons.append(Moon(int(m.group("x")), int(m.group("y")), int(m.group("z"))))
    return moons


def apply_gravity_across(moons):
    for moon in moons:
        for other_moon in moons:
            if moon is other_moon:
                continue
            moon.apply_gravity(other_moon)


def get_total_energy(moons):
    return sum([m.potential_energy * m.kinetic_energy for m in moons])


if __name__ == "__main__":
    with open("inputs/day12_input", "r") as f:
        read_lines = f.read().strip().split("\n")

    def part1():
        moons = get_moons(read_lines)
        for _ in range(1000):
            apply_gravity_across(moons)
            for moon in moons:
                moon.apply_velocity()
        assert 7013 == get_total_energy(moons)

    def part2():
        moons = get_moons(read_lines)
        orig_state = {
            coord: [(getattr(m, coord), getattr(m, f"d{coord}")) for m in moons]
            for coord in "xyz"
        }
        time_step = 0
        cycle_lengths = {}
        while len(cycle_lengths) < 3:
            apply_gravity_across(moons)
            for moon in moons:
                moon.apply_velocity()
            time_step += 1
            for moon_idx in range(4):
                for coord in "xyz":
                    if coord in cycle_lengths:
                        continue
                    dcoord = f"d{coord}"
                    curr_coord_state = [
                        (getattr(m, coord), getattr(m, dcoord)) for m in moons
                    ]
                    if curr_coord_state == orig_state[coord]:
                        cycle_lengths[coord] = time_step
        assert 324618307124784 == lcm(list(cycle_lengths.values()))

    part1()
    part2()
