#!/usr/bin/env python3

from math import gcd, degrees, atan2


def normalize(dx, dy):
    g = gcd(dx, dy)
    return (dx / g, dy / g)


class Asteroid:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def position_relative_to(self, other_asteroid):
        return (self.x - other_asteroid.x, self.y - other_asteroid.y)


def get_asteroids_detectable_from(asteroid, all_asteroids):
    other_asteroids = [
        (a, a.position_relative_to(asteroid))
        for a in all_asteroids
        if not a is asteroid
    ]
    normalized_positions_to_asteroids = {}
    for other_asteroid, relative_pos in other_asteroids:
        normalized_pos = normalize(*relative_pos)
        if normalized_pos in normalized_positions_to_asteroids:
            existing_other_asteroid = normalized_positions_to_asteroids[normalized_pos]
            if other_asteroid.x < existing_other_asteroid.x:
                normalized_positions_to_asteroids[normalized_pos] = other_asteroid
        else:
            normalized_positions_to_asteroids[normalized_pos] = other_asteroid
    return normalized_positions_to_asteroids.values()


def sort_asteroids_clockwise(asteroid, other_asteroids):
    def polar_angle(other_asteroid):
        dx, dy = other_asteroid.position_relative_to(asteroid)
        ds = degrees(atan2(dx, -dy))
        return ds if ds >= 0 else 360 + ds

    return sorted(other_asteroids, key=polar_angle)


def get_asteroids(read_lines):
    asteroids = []
    for row_idx, row in enumerate(read_lines):
        for col_idx, element in enumerate(row):
            if element == "#":
                asteroids.append(Asteroid(col_idx, row_idx))
    return asteroids


def get_pos_of_monitoring_station(asteroids):
    best_pos = None
    max_count_of_detectable_asteroids = None
    for curr_asteroid in asteroids:
        detectables = get_asteroids_detectable_from(curr_asteroid, asteroids)
        count_of_detectable_asteroids = len(detectables)
        if (
            max_count_of_detectable_asteroids is None
            or count_of_detectable_asteroids > max_count_of_detectable_asteroids
        ):
            best_pos = curr_asteroid
            max_count_of_detectable_asteroids = count_of_detectable_asteroids
    return best_pos, max_count_of_detectable_asteroids


def get_200th_vaporized_asteroid(pos_of_monitoring_station, asteroids):
    asteroids_pos = set(
        [(a.x, a.y) for a in asteroids if not a is pos_of_monitoring_station]
    )
    number_of_asteroids_vaporized = 0
    while True:
        asteroids = [Asteroid(p[0], p[1]) for p in asteroids_pos]
        detectables = get_asteroids_detectable_from(
            pos_of_monitoring_station, asteroids
        )
        detectables_pos = set([(a.x, a.y) for a in detectables])
        if number_of_asteroids_vaporized + len(detectables_pos) < 200:
            number_of_asteroids_vaporized += len(detectables_pos)
            asteroids_pos -= detectables_pos
        else:
            detectables = sort_asteroids_clockwise(
                pos_of_monitoring_station, detectables
            )
            return detectables[200 - number_of_asteroids_vaporized - 1]


if __name__ == "__main__":
    with open("inputs/day10_input", "r") as f:
        lines = f.read().strip().split("\n")
    asteroids = get_asteroids(lines)
    (
        pos_of_monitoring_station,
        max_count_of_detectable_asteroids,
    ) = get_pos_of_monitoring_station(asteroids)
    assert 276 == max_count_of_detectable_asteroids
    lucky_asteroid = get_200th_vaporized_asteroid(pos_of_monitoring_station, asteroids)
    assert 1321 == 100 * lucky_asteroid.x + lucky_asteroid.y
