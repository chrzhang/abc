#!/usr/bin/env python3
import re

class Moon:
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z
        self.dx, self.dy, self.dz = (0, 0, 0)
    def apply_gravity(self, other_moon):
        for coord in 'xyz':
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
    moons = get_moons(read_lines)
    number_of_time_steps = 1000
    time_step = 0
    while time_step < number_of_time_steps:
        apply_gravity_across(moons)
        for moon in moons:
            moon.apply_velocity()
        time_step += 1
    assert 7013 == get_total_energy(moons)



