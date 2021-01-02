#!/usr/bin/env python3


class Eris:
    def __init__(self, grid):
        self.grid = [list(r) for r in grid]
        self.height = len(grid)
        self.width = len(grid[0])

    def get_neighbors(self, row, col):
        for dr, dc in [(-1, 0), (1, 0), (0, 1), (0, -1)]:
            if 0 <= row + dr < self.height and 0 <= col + dc < self.width:
                yield self.grid[row + dr][col + dc]

    def elapse(self):
        next_grid = [row[:] for row in self.grid]
        for row in range(0, self.height):
            for col in range(0, self.width):
                is_bug = self.grid[row][col] == "#"
                neighbors = self.get_neighbors(row, col)
                number_of_bug_neighbors = len([n for n in neighbors if n == "#"])
                if is_bug:
                    if number_of_bug_neighbors == 1:
                        next_grid[row][col] = "#"
                    else:
                        next_grid[row][col] = "."
                else:
                    if number_of_bug_neighbors in (1, 2):
                        next_grid[row][col] = "#"
                    else:
                        next_grid[row][col] = "."
        self.grid = next_grid

    def serialized(self):
        return int("".join(["1" if e == "#" else "0" for r in self.grid for e in r]), 2)

    @classmethod
    def deserialize(cls, serial, height, width):
        as_bits = f"{serial:0{height*width}b}"
        as_bugs = ["#" if b == "1" else "." for b in as_bits]
        as_rows = [as_bugs[i : i + width] for i in range(0, height * width, width)]
        return cls(["".join(r) for r in as_rows])

    def biodiversity_rating(self):
        rating = 0
        for row in range(self.height):
            for col in range(self.width):
                i = row * self.width + col
                if self.grid[row][col] == "#":
                    rating += 2 ** i
        return rating


class PlutonianEris:
    def __init__(self, grid):
        layer = 0
        self.bugs = set()
        for row in range(5):
            for col in range(5):
                if grid[row][col] == "#":
                    self.bugs.add((layer, row, col))

    def get_neighbors(self, layer, row, col):
        neighbors = set()
        for (dr, dc) in ((1, 0), (-1, 0), (0, 1), (0, -1)):
            if 0 <= row + dr < 5 and 0 <= col + dc < 5:
                if (row + dr, col + dc) == (2, 2):
                    if row == 1:
                        neighbors |= {(layer + 1, 0, c) for c in range(5)}
                    elif row == 3:
                        neighbors |= {(layer + 1, 4, c) for c in range(5)}
                    elif col == 1:
                        neighbors |= {(layer + 1, r, 0) for r in range(5)}
                    elif col == 3:
                        neighbors |= {(layer + 1, r, 4) for r in range(5)}
                else:
                    neighbors.add((layer, row + dr, col + dc))
            else:
                if row + dr < 0:
                    neighbors.add((layer - 1, 1, 2))
                elif row + dr >= 5:
                    neighbors.add((layer - 1, 3, 2))
                elif col + dc < 0:
                    neighbors.add((layer - 1, 2, 1))
                elif col + dc >= 5:
                    neighbors.add((layer - 1, 2, 3))
        return neighbors

    def all_bug_neighbors(self):
        neighbors = set()
        for (bug_layer, bug_row, bug_col) in self.bugs:
            neighbors |= self.get_neighbors(bug_layer, bug_row, bug_col)
        return neighbors - self.bugs

    def elapse(self):
        next_bugs = set()
        for (bug_layer, bug_row, bug_col) in self.bugs:
            neighbors = self.get_neighbors(bug_layer, bug_row, bug_col)
            if len([n for n in neighbors if n in self.bugs]) == 1:
                next_bugs.add((bug_layer, bug_row, bug_col))
        for (empty_layer, empty_row, empty_col) in self.all_bug_neighbors():
            neighbors = self.get_neighbors(empty_layer, empty_row, empty_col)
            if len([n for n in neighbors if n in self.bugs]) in (1, 2):
                next_bugs.add((empty_layer, empty_row, empty_col))
        self.bugs = next_bugs


if __name__ == "__main__":
    with open("inputs/day24_input", "r") as f:
        grid = f.read().strip().split("\n")

    def part1():
        e = Eris(grid)

        layouts_so_far = set()
        while True:
            e.elapse()

            serialized = e.serialized()
            if serialized in layouts_so_far:
                break
            layouts_so_far.add(serialized)

        e = Eris.deserialize(serialized, len(grid), len(grid[0]))
        return e.biodiversity_rating()

    assert 18370591 == part1()

    def part2():
        e = PlutonianEris(grid)
        for _ in range(200):
            e.elapse()
        return len(e.bugs)

    assert 2040 == part2()