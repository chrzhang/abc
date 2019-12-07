#!/usr/bin/python3

with open("inputs/day3_input", "r") as f:
    lines = f.read().strip().split("\n")

wire_1 = lines[0].split(",")
wire_2 = lines[1].split(",")


class FixedXRange:
    def __init__(self, x, y_from, y_to):
        self.x = x
        self.y_from = y_from
        self.y_to = y_to

    def points(self):
        y = self.y_from
        while y != self.y_to:
            yield (self.x, y)
            if self.y_to > self.y_from:
                y += 1
            else:
                y -= 1
        yield (self.x, y)


class FixedYRange:
    def __init__(self, y, x_from, x_to):
        self.y = y
        self.x_from = x_from
        self.x_to = x_to

    def points(self):
        x = self.x_from
        while x != self.x_to:
            yield (x, self.y)
            if self.x_to > self.x_from:
                x += 1
            else:
                x -= 1
        yield (x, self.y)


def get_wire_positions(wire_path):
    ranges = []
    curr_x, curr_y = 0, 0
    for path in wire_path:
        dist = int(path[1:])
        if path[0] == "U":
            ranges.append(FixedXRange(curr_x, curr_y, curr_y + dist))
            curr_y += dist
        elif path[0] == "R":
            ranges.append(FixedYRange(curr_y, curr_x, curr_x + dist))
            curr_x += dist
        elif path[0] == "D":
            ranges.append(FixedXRange(curr_x, curr_y, curr_y - dist))
            curr_y -= dist
        elif path[0] == "L":
            ranges.append(FixedYRange(curr_y, curr_x, curr_x - dist))
            curr_x -= dist
        else:
            raise RuntimeError(f"Unknown path {path}")
    return ranges


def flatten(forest):
    return [leaf for tree in forest for leaf in tree]


def get_intersections(ranges_1, ranges_2):
    points_1 = set(flatten([list(r.points()) for r in ranges_1]))
    for r in ranges_2:
        for point_2 in r.points():
            if point_2 in points_1:
                yield point_2


def get_shortest_intersection_distance(wire_path_1, wire_path_2):
    wire_positions_1 = get_wire_positions(wire_path_1)
    wire_positions_2 = get_wire_positions(wire_path_2)
    intersections = get_intersections(wire_positions_1, wire_positions_2)
    least_distance = None
    for intersection in intersections:
        if intersection == (0, 0):
            continue
        potential_least_distance = abs(intersection[0]) + abs(intersection[1])
        if least_distance is None:
            least_distance = potential_least_distance
        else:
            least_distance = min(least_distance, potential_least_distance)
    return least_distance


shortest_distance = get_shortest_intersection_distance(wire_1, wire_2)

assert 293 == shortest_distance


def get_points_to_dists(some_range):
    points = [list(r.points()) for r in some_range]
    for point_list in points[:-1]:
        point_list.pop()
    return {x[1]: x[0] for x in list(enumerate(flatten(points)))}


def get_intersections_with_dists(points_to_dists_1, points_to_dists_2):
    intersections = []
    for point_1, dist_1 in points_to_dists_1.items():
        if point_1 in points_to_dists_2:
            intersections.append(dist_1 + points_to_dists_2[point_1])
    return intersections


def get_shortest_intersection_path(wire_path_1, wire_path_2):
    wire_positions_1 = get_wire_positions(wire_path_1)
    wire_positions_2 = get_wire_positions(wire_path_2)
    points_to_dists_1 = get_points_to_dists(wire_positions_1)
    points_to_dists_2 = get_points_to_dists(wire_positions_2)
    intersection_dists = get_intersections_with_dists(
        points_to_dists_1, points_to_dists_2
    )
    least_dist = None
    for dist in intersection_dists:
        if dist == 0:
            continue
        if least_dist is None:
            least_dist = dist
        else:
            least_dist = min(least_dist, dist)
    return least_dist


result = get_shortest_intersection_path(wire_1, wire_2)
assert 27306 == result
