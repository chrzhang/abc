#!/usr/bin/env python3

from math import gcd

def normalize(dx, dy):
    g = gcd(dx, dy)
    return (dx / g, dy / g)

class Asteroid:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def position_relative_to(self, other_asteroid):
        return (self.x - other_asteroid.x, self.y - other_asteroid.y)

    def __str__(self):
        return f"Asteroid @ ({self.x}, {self.y})"


def get_asteroids(read_lines):
    asteroids = []
    for row_idx, row in enumerate(read_lines):
        for col_idx, element in enumerate(row):
            if element == "#":
                asteroids.append(Asteroid(col_idx, row_idx))
    return asteroids


if __name__ == "__main__":
    with open("inputs/day10_input", "r") as f:
        lines = f.read().strip().split("\n")
    asteroids = get_asteroids(lines)
    max_count_of_detectable_asteroids = None
    for curr_asteroid in asteroids:
        other_asteroids = [a for a in asteroids if not a is curr_asteroid]
        relative_positions_of_other_asteroids = [
            a.position_relative_to(curr_asteroid) for a in other_asteroids
        ]
        normalized_relative_positions = [
            normalize(*p) for p in relative_positions_of_other_asteroids
        ]
        count_of_detectable_asteroids = len(set(normalized_relative_positions))
        if max_count_of_detectable_asteroids is None or count_of_detectable_asteroids > max_count_of_detectable_asteroids:
            max_count_of_detectable_asteroids = count_of_detectable_asteroids
    assert 276 == max_count_of_detectable_asteroids




