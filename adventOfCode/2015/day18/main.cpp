#include <iostream>
#include <fstream>
#include <string>
#include <cassert>
#include <vector>

/*
From http://adventofcode.com/2015/day/18

--- Day 18: Like a GIF For Your Yard ---

After the million lights incident, the fire code has gotten stricter: now, at
most ten thousand lights are allowed. You arrange them in a 100x100 grid.

Never one to let you down, Santa again mails you instructions on the ideal
lighting configuration. With so few lights, he says, you'll have to resort to
animation.

Start by setting your lights to the included initial configuration (your puzzle
input). A # means "on", and a . means "off".

Then, animate your grid in steps, where each step decides the next
configuration based on the current one. Each light's next state (either on or
off) depends on its current state and the current states of the eight lights
adjacent to it (including diagonals). Lights on the edge of the grid might have
fewer than eight neighbors; the missing ones always count as "off".

For example, in a simplified 6x6 grid, the light marked A has the neighbors
numbered 1 through 8, and the light marked B, which is on an edge, only has the
neighbors marked 1 through 5:

1B5...
234...
......
..123.
..8A4.
..765.

The state a light should have next is based on its current state (on or off)
plus the number of neighbors that are on:

A light which is on stays on when 2 or 3 neighbors are on, and turns off
otherwise.  A light which is off turns on if exactly 3 neighbors are on, and
stays off otherwise.  All of the lights update simultaneously; they all
consider the same current state before moving to the next.

Here's a few steps from an example configuration of another 6x6 grid:

Initial state:
.#.#.#
...##.
#....#
..#...
#.#..#
####..

After 1 step:
..##..
..##.#
...##.
......
#.....
#.##..

After 2 steps:
..###.
......
..###.
......
.#....
.#....

After 3 steps:
...#..
......
...#..
..##..
......
......

After 4 steps:
......
......
..##..
..##..
......
......

After 4 steps, this example has four lights on.

In your grid of 100x100 lights, given your initial configuration, how many
lights are on after 100 steps?

--- Part Two ---

You flip the instructions over; Santa goes on to point out that this is all
just an implementation of Conway's Game of Life. At least, it was, until you
notice that something's wrong with the grid of lights you bought: four lights,
one in each corner, are stuck on and can't be turned off. The example above
will actually run like this:

Initial state:
##.#.#
...##.
#....#
..#...
#.#..#
####.#

After 1 step:
#.##.#
####.#
...##.
......
#...#.
#.####

After 2 steps:
#..#.#
#....#
.#.##.
...##.
.#..##
##.###

After 3 steps:
#...##
####.#
..##.#
......
##....
####.#

After 4 steps:
#.####
#....#
...#..
.##...
#.....
#.#..#

After 5 steps:
##.###
.##..#
.##...
.##...
#.#...
##...#

After 5 steps, this example now has 17 lights on.

In your grid of 100x100 lights, given your initial configuration, but with the
four corners always in the on state, how many lights are on after 100 steps?
*/

const int HEIGHT = 100;
const int WIDTH = 100;
const int NUM_ANIMATIONS = 100;

int numNeighbors(bool turnedOn, int row, int col,
                 const std::vector<std::vector<bool>> & lightsOn) {
    int num = 0;
    assert(row >= 0 & row < lightsOn.size());
    assert(col >= 0 & col < lightsOn[row].size());
    // Whether cell has neighbors in those directions
    bool hasNorth = row != 0;
    bool hasEast = col != lightsOn[row].size() - 1;
    bool hasWest = col != 0;
    bool hasSouth = row != lightsOn.size() - 1;
    // North
    if (hasNorth) {
        num += lightsOn[row - 1][col] == turnedOn;
    }
    // East
    if (hasEast) {
        num += lightsOn[row][col + 1] == turnedOn;
    }
    // West
    if (hasWest) {
        num += lightsOn[row][col - 1] == turnedOn;
    }
    // South
    if (hasSouth) {
        num += lightsOn[row + 1][col] == turnedOn;
    }
    // Northeast
    if (hasNorth && hasEast) {
        num += lightsOn[row - 1][col + 1] == turnedOn;
    }
    // Northwest
    if (hasNorth && hasWest) {
        num += lightsOn[row - 1][col - 1] == turnedOn;
    }
    // Southeast
    if (hasSouth && hasEast) {
        num += lightsOn[row + 1][col + 1] == turnedOn;
    }
    // Southwest
    if (hasSouth && hasWest) {
        num += lightsOn[row + 1][col - 1] == turnedOn;
    }
    return num;
}

void animate(std::vector<std::vector<bool>> & lightsOn) {
    auto temp = lightsOn;
    for (int row = 0; row < lightsOn.size(); ++row) {
        for (int col = 0; col < lightsOn[row].size(); ++col) {
            auto numNeighborsOn = numNeighbors(true, row, col, lightsOn);
            if (lightsOn[row][col]) {
                // Turn off if 2 or 3 neighbors are not on
                if (numNeighborsOn != 2 && numNeighborsOn != 3) {
                    temp[row][col] = false;
                }
            } else {
                // Turn on if 3 neighbors are on
                if (numNeighborsOn == 3) {
                    temp[row][col] = true;
                }
            }
        }
    }
    lightsOn = temp;
}

int numLightsOn(const std::vector<std::vector<bool>> & lightsOn) {
    int counter = 0;
    for (auto row : lightsOn) {
        for (auto lit : row) {
            counter += lit;
        }
    }
    return counter;
}

void part1(std::vector<std::vector<bool>> lightsOn) {
    for (int i = 0; i < NUM_ANIMATIONS; ++i) {
        animate(lightsOn);
    }
    std::cout << "Part 1: " << numLightsOn(lightsOn) << " lights are on.\n";
}

void override(std::vector<std::vector<bool>> & lightsOn) {
    lightsOn[0][0] =
    lightsOn[0][WIDTH - 1] =
    lightsOn[HEIGHT - 1][0] =
    lightsOn[HEIGHT - 1][WIDTH - 1] = true;
}

void part2(std::vector<std::vector<bool>> lightsOn) {
    for (int i = 0; i < NUM_ANIMATIONS; ++i) {
        override(lightsOn);
        animate(lightsOn);
        override(lightsOn);
    }
    std::cout << "Part 2: " << numLightsOn(lightsOn) << " lights are on.\n";
}

int main(int argc, char * argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::ifstream f(argv[1]);
    if (!f.is_open()) {
        std::cerr << argv[1] << " not opened.\n";
        return 1;
    }
    std::string line;
    std::vector<std::vector<bool>> lightsOn;
    while (std::getline(f, line)) {
        assert(line.size() == WIDTH);
        std::vector<bool> row;
        for (auto c : line) {
            if (c == '#') {
                row.push_back(true);
            } else {
                assert(c == '.');
                row.push_back(false);
            }
        }
        lightsOn.push_back(row);
    }
    assert(lightsOn.size() == HEIGHT);
    part1(lightsOn);
    part2(lightsOn);
    return 0;
}
