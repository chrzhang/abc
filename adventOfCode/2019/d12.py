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
        number_of_time_steps = 1000
        time_step = 0
        while time_step < number_of_time_steps:
            apply_gravity_across(moons)
            for moon in moons:
                moon.apply_velocity()
            time_step += 1
        assert 7013 == get_total_energy(moons)

    def part2():
        moons = get_moons(read_lines)
        x_orig = [(m.x, m.dx) for m in moons]
        y_orig = [(m.y, m.dy) for m in moons]
        z_orig = [(m.z, m.dz) for m in moons]
        time_step = 0
        x_cycle_length = None
        y_cycle_length = None
        z_cycle_length = None
        while True:
            apply_gravity_across(moons)
            for moon in moons:
                moon.apply_velocity()
            time_step += 1
            for moon_idx in range(4):
                if [(m.x, m.dx) for m in moons] == x_orig:
                    x_cycle_length = x_cycle_length or time_step
                if [(m.y, m.dy) for m in moons] == y_orig:
                    y_cycle_length = y_cycle_length or time_step
                if [(m.z, m.dz) for m in moons] == z_orig:
                    z_cycle_length = z_cycle_length or time_step
            if x_cycle_length and y_cycle_length and z_cycle_length:
                break
        assert 324618307124784 == lcm([x_cycle_length, y_cycle_length, z_cycle_length])

    part1()
    part2()
