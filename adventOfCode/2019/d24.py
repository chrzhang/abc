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


if __name__ == "__main__":
    with open("inputs/day24_input", "r") as f:
        grid = f.read().strip().split("\n")
    e = Eris(grid)

    layouts_so_far = set()
    while True:
        e.elapse()

        serialized = e.serialized()
        if serialized in layouts_so_far:
            break
        layouts_so_far.add(serialized)

    e = Eris.deserialize(serialized, len(grid), len(grid[0]))
    assert 18370591 == e.biodiversity_rating()
