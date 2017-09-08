#include <iostream>
#include <cassert>
#include <queue>
#include <map>

#define FAVORITE_NUMBER 1350

/*
From http://adventofcode.com/2016/day/13

--- Day 13: A Maze of Twisty Little Cubicles ---

You arrive at the first floor of this new building to discover a much less
welcoming environment than the shiny atrium of the last one. Instead, you are
in a maze of twisty little cubicles, all alike.

Every location in this area is addressed by a pair of non-negative integers
(x,y). Each such coordinate is either a wall or an open space. You can't move
diagonally. The cube maze starts at 0,0 and seems to extend infinitely toward
positive x and y; negative values are invalid, as they represent a location
outside the building. You are in a small waiting area at 1,1.

While it seems chaotic, a nearby morale-boosting poster explains, the layout is
actually quite logical. You can determine whether a given x,y coordinate will
be a wall or an open space using a simple system:

Find x*x + 3*x + 2*x*y + y + y*y.
Add the office designer's favorite number (your puzzle input).
Find the binary representation of that sum; count the number of bits that are 1.
If the number of bits that are 1 is even, it's an open space.
If the number of bits that are 1 is odd, it's a wall.

For example, if the office designer's favorite number were 10, drawing walls as
# and open spaces as ., the corner of the building containing 0,0 would look
like this:

  0123456789
0 .#.####.##
1 ..#..#...#
2 #....##...
3 ###.#.###.
4 .##..#..#.
5 ..##....#.
6 #...##.###

Now, suppose you wanted to reach 7,4. The shortest route you could take is
marked as O:

  0123456789
0 .#.####.##
1 .O#..#...#
2 #OOO.##...
3 ###O#.###.
4 .##OO#OO#.
5 ..##OOO.#.
6 #...##.###

Thus, reaching 7,4 would take a minimum of 11 steps (starting from your current
location, 1,1).

What is the fewest number of steps required for you to reach 31,39?

--- Part Two ---

How many locations (distinct x,y coordinates, including your starting location)
can you reach in at most 50 steps?
*/

bool hasOddNumberOfBitsSet(unsigned n) {
    bool parity = false;
    while (n) {
        parity = !parity;
        n = n & (n - 1);
    }
    return parity;
}

bool isWall(unsigned x, unsigned y) {
    const unsigned transformed =
        x * x + 3 * x + 2 * x * y + y + y * y + FAVORITE_NUMBER;
    return hasOddNumberOfBitsSet(transformed);
}

struct LocationData {
    unsigned x, y;
    unsigned depth;
    LocationData(unsigned x, unsigned y, unsigned d)
    : x(x), y(y), depth(d)
    {}
};

unsigned countStepsTo(const unsigned x, const unsigned y) {
    // Brute force breadth first search
    std::map<unsigned, std::map<unsigned, bool>> visited;
    std::queue<LocationData> queue;
    visited[1][1] = true;
    queue.push(LocationData(1, 1, 0));
    while (!queue.empty()) {
        const LocationData currentLoc = queue.front();
        queue.pop();
        if (currentLoc.x == x && currentLoc.y == y) {
            return currentLoc.depth;
        }
        std::vector<LocationData> neighbors = {};
        if (currentLoc.y > 0) { // Add north
            neighbors.push_back(LocationData(currentLoc.x,
                                             currentLoc.y - 1,
                                             currentLoc.depth + 1));
        }
        if (currentLoc.x > 0) { // Add west
            neighbors.push_back(LocationData(currentLoc.x - 1,
                                             currentLoc.y,
                                             currentLoc.depth + 1));
        }
        neighbors.push_back(LocationData(currentLoc.x + 1, currentLoc.y,
                                         currentLoc.depth + 1));
        neighbors.push_back(LocationData(currentLoc.x, currentLoc.y + 1,
                                         currentLoc.depth + 1));
        for (const auto & neighbor : neighbors) {
            if (!isWall(neighbor.x, neighbor.y)) {
                if (visited[neighbor.x].find(neighbor.y) ==
                    visited[neighbor.x].end()) {
                    // Unvisited
                    visited[neighbor.x][neighbor.y] = true;
                    queue.push(neighbor);
                }
            }
        }
    }
    return 0;
}

unsigned numberOfReachableLocationsWithin(unsigned numberOfSteps) {
    std::map<unsigned, std::map<unsigned, bool>> visited;
    std::queue<LocationData> queue;
    visited[1][1] = true;
    queue.push(LocationData(1, 1, 0));
    while (!queue.empty()) {
        const LocationData currentLoc = queue.front();
        queue.pop();
        std::vector<LocationData> neighbors = {};
        if (currentLoc.y > 0) { // Add north
            neighbors.push_back(LocationData(currentLoc.x, currentLoc.y - 1,
                                             currentLoc.depth + 1));
        }
        if (currentLoc.x > 0) { // Add west
            neighbors.push_back(LocationData(currentLoc.x - 1, currentLoc.y,
                                             currentLoc.depth + 1));
        }
        neighbors.push_back(LocationData(currentLoc.x + 1, currentLoc.y,
                                         currentLoc.depth + 1));
        neighbors.push_back(LocationData(currentLoc.x, currentLoc.y + 1,
                                         currentLoc.depth + 1));
        for (const auto & neighbor : neighbors) {
            if (!isWall(neighbor.x, neighbor.y) &&
                (visited[neighbor.x].find(neighbor.y) == visited[neighbor.x].end()) &&
                neighbor.depth <= numberOfSteps) {
                // Unvisited and within depth
                visited[neighbor.x][neighbor.y] = true;
                queue.push(neighbor);
            }
        }
    }
    unsigned counter = 0;
    // Count the number of visited cells in visited
    for (const auto & rowMapIter : visited) {
        for (const auto & colMapIter : rowMapIter.second) {
            if (colMapIter.second) {
                ++counter;
            }
        }
    }
    return counter;
}

int main() {
    const auto part1Result = countStepsTo(31, 39);
    assert(92 == part1Result);
    std::cout << "Part 1: " << part1Result << std::endl;
    const auto part2Result = numberOfReachableLocationsWithin(50);
    assert(124 == part2Result);
    std::cout << "Part 2: " << part2Result << std::endl;
    return 0;
}
