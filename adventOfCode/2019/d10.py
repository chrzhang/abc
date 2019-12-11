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

def get_asteroids_detectable_from(asteroid, all_asteroids):
    other_asteroids = [
        (a, a.position_relative_to(asteroid))
        for a in all_asteroids if not a is asteroid
    ]
    normalized_positions_to_asteroids = {}
    for other_asteroid, relative_pos in other_asteroids:
        normalized_pos = normalize(*relative_pos)
        if normalized_pos in normalized_positions_to_asteroids:
            existing_other_asteroid = normalized_positions_to_asteroids[
                normalized_pos
            ]
            if other_asteroid.x < existing_other_asteroid.x:
                normalized_positions_to_asteroids[
                    normalized_pos
                ] = other_asteroid
        else:
            normalized_positions_to_asteroids[
                normalized_pos
            ] = other_asteroid
    return normalized_positions_to_asteroids.values()


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
        detectables = get_asteroids_detectable_from(curr_asteroid, asteroids)
        count_of_detectable_asteroids = len(detectables)
        if max_count_of_detectable_asteroids is None or count_of_detectable_asteroids > max_count_of_detectable_asteroids:
            max_count_of_detectable_asteroids = count_of_detectable_asteroids
    assert 276 == max_count_of_detectable_asteroids




