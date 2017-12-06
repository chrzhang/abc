#include <iostream>
#include <iomanip>
#include <sstream>
#include <fstream>
#include <cassert>
#include <climits>
#include <string>
#include <vector>
#include <algorithm>

/*
From http://adventofcode.com/2016/day/24

--- Day 24: Air Duct Spelunking ---

You've finally met your match; the doors that provide access to the roof are
locked tight, and all of the controls and related electronics are inaccessible.
You simply can't reach them.

The robot that cleans the air ducts, however, can.

It's not a very fast little robot, but you reconfigure it to be able to
interface with some of the exposed wires that have been routed through the HVAC
system. If you can direct it to each of those locations, you should be able to
bypass the security controls.

You extract the duct layout for this area from some blueprints you acquired and
create a map with the relevant locations marked (your puzzle input). 0 is your
current location, from which the cleaning robot embarks; the other numbers are
(in no particular order) the locations the robot needs to visit at least once
each. Walls are marked as #, and open passages are marked as .. Numbers behave
like open passages.

For example, suppose you have a map like the following:

###########
#0.1.....2#
#.#######.#
#4.......3#
###########

To reach all of the points of interest as quickly as possible, you would have
the robot take the following path:

0 to 4 (2 steps)
4 to 1 (4 steps; it can't move diagonally)
1 to 2 (6 steps)
2 to 3 (2 steps)

Since the robot isn't very fast, you need to find it the shortest route. This
path is the fewest steps (in the above example, a total of 14) required to
start at 0 and then visit every other location at least once.

Given your actual map, and starting from location 0, what is the fewest number
of steps required to visit every non-0 number marked on the map at least once?

--- Part Two ---

Of course, if you leave the cleaning robot somewhere weird, someone is bound to
notice.

What is the fewest number of steps required to start at 0, visit every non-0
number marked on the map at least once, and then return to 0?
*/

class Location {
    size_t d_row, d_column, d_index;
    public:
        Location(const size_t row, const size_t column, const size_t index)
        : d_row(row), d_column(column), d_index(index) {
        }
        size_t row() const {
            return d_row;
        }
        size_t column() const {
            return d_column;
        }
        size_t index() const {
            return d_index;
        }
};

bool operator<(const Location & l1, const Location & l2) {
    if (l1.row() < l2.row()) {
        return true;
    }
    if (l1.row() == l2.row() && l1.column() < l2.column()) {
        return true;
    }
    return false;
}

std::vector<std::vector<int>>
getDistancesBetweenTargets(const std::vector<Location> & targets,
                           const std::vector<std::vector<int>> maze,
                           const size_t numVertices) {
    // Use Floyd-Warshall algorithm to get distance from each target to every
    // other target
    // Initialize a |V| x |V| array of minimum distances
    std::vector<std::vector<int>> result(numVertices,
                                         std::vector<int>(numVertices,
                                                          INT_MAX));
    for (size_t vertexIndex = 0; vertexIndex < numVertices; ++vertexIndex) {
        result[vertexIndex][vertexIndex] = 0;
    }
    // For each edge
    for (size_t row = 0; row < maze.size(); ++row) {
        for (size_t col = 0; col < maze[row].size(); ++col) {
            const int currentVertexIndex = maze[row][col];
            if (currentVertexIndex == -1) {
                continue;
            }
            // North
            if (row >= 1) {
                const int northVertexIndex = maze[row - 1][col];
                if (northVertexIndex != -1) {
                    result[currentVertexIndex][northVertexIndex] = 1;
                    result[northVertexIndex][currentVertexIndex] = 1;
                }
            }
            // East
            if (col + 1 < maze[row].size()) {
                const int eastVertexIndex = maze[row][col + 1];
                if (eastVertexIndex != -1) {
                    result[currentVertexIndex][eastVertexIndex] = 1;
                    result[eastVertexIndex][currentVertexIndex] = 1;
                }
            }
            // South
            if (row + 1 < maze.size()) {
                const int southVertexIndex = maze[row + 1][col];
                if (southVertexIndex != -1) {
                    result[currentVertexIndex][southVertexIndex] = 1;
                    result[southVertexIndex][currentVertexIndex] = 1;
                }
            }
            // West
            if (col >= 1) {
                const int westVertexIndex = maze[row][col - 1];
                if (westVertexIndex != -1) {
                    result[currentVertexIndex][westVertexIndex] = 1;
                    result[westVertexIndex][currentVertexIndex] = 1;
                }
            }
        }
    }
    for (size_t k = 0; k < numVertices; ++k) {
        for (size_t i = 0; i < numVertices; ++i) {
            for (size_t j = 0; j < numVertices; ++j) {
                if (result[i][k] != INT_MAX && result[k][j] != INT_MAX) {
                    if (result[i][j] > result[i][k] + result[k][j]) {
                        result[i][j] = result[i][k] + result[k][j];
                    }
                }
            }
        }
    }
    return result;
}

unsigned day24a_solve(std::vector<Location> targets,
                      const std::vector<std::vector<int>> maze,
                      const size_t numVertices) {
    const std::vector<std::vector<int>> distancesBetweenTargets =
        getDistancesBetweenTargets(targets, maze, numVertices);
    // Will be considering every possible visit of ordering all targets and
    // picking the most optimal order to visit them
    unsigned leastAmountOfStepsSoFar = UINT_MAX;
    do {
        unsigned stepCounter = 0;
        for (size_t ii = 1; ii < targets.size() &&
                            stepCounter < leastAmountOfStepsSoFar; ++ii) {
            stepCounter += distancesBetweenTargets[targets[ii - 1].index()]
                                                  [targets[ii].index()];
        }
        leastAmountOfStepsSoFar =
            std::min(leastAmountOfStepsSoFar, stepCounter);
    } while (std::next_permutation(std::next(targets.begin()), targets.end()));
    return leastAmountOfStepsSoFar;
}

unsigned day24b_solve(std::vector<Location> targets,
                      const std::vector<std::vector<int>> maze,
                      const size_t numVertices) {
    targets.push_back(*targets.begin());
    const std::vector<std::vector<int>> distancesBetweenTargets =
        getDistancesBetweenTargets(targets, maze, numVertices);
    // Will be considering every possible visit of ordering all targets and
    // picking the most optimal order to visit them
    unsigned leastAmountOfStepsSoFar = UINT_MAX;
    do {
        unsigned stepCounter = 0;
        for (size_t ii = 1; ii < targets.size() &&
                            stepCounter < leastAmountOfStepsSoFar; ++ii) {
            stepCounter +=
                distancesBetweenTargets[targets[ii - 1].index()]
                                       [targets[ii].index()];
        }
        leastAmountOfStepsSoFar = std::min(leastAmountOfStepsSoFar,
                                           stepCounter);
    } while (std::next_permutation(std::next(targets.begin()),
                                   std::prev(targets.end())));
    return leastAmountOfStepsSoFar;
}

int solve(const char * filename, const char part) {
    std::ifstream inputFile(filename);
    if (!inputFile.is_open()) {
        std::cerr << "Cannot open " << filename << std::endl;
        return 1;
    }
    std::string currentLine;
    std::vector<std::vector<int>> maze;
    std::vector<Location> targets;
    size_t vertexCount = 0;
    size_t rowIndex = 0;
    while (std::getline(inputFile, currentLine)) {
        std::vector<int> currentRow;
        for (size_t colIndex = 0; colIndex < currentLine.size(); ++colIndex) {
            if (currentLine[colIndex] == '#') {
                currentRow.push_back(-1);
            } else if (currentLine[colIndex] == '.') {
                currentRow.push_back(vertexCount);
                ++vertexCount;
            } else {
                assert (currentLine[colIndex] >= '0' &&
                        currentLine[colIndex] <= '9');
                if (currentLine[colIndex] == '0') {
                    targets.insert(targets.begin(),
                                   Location(rowIndex, colIndex, vertexCount));
                } else {
                    targets.push_back(Location(rowIndex, colIndex,
                                               vertexCount));
                }
                currentRow.push_back(vertexCount);
                ++vertexCount;
            }
        }
        maze.push_back(currentRow);
        ++rowIndex;
    }
    inputFile.close();
    if ('A' == part) {
        return day24a_solve(targets, maze, vertexCount);
    } else if ('B' == part) {
        return day24b_solve(targets, maze, vertexCount);
    } else {
        return -1;
    }
}

int main() {
    const int sample_output = solve("sample.txt", 'A');
    assert (sample_output == 14);
    const int day24a_output = solve("input.txt", 'A');
    std::cout << "Part 1: " << day24a_output << std::endl;
    assert (day24a_output == 456);
    const int day24b_output = solve("input.txt", 'B');
    std::cout << "Part 2: " << day24b_output << std::endl;
    assert (day24b_output == 704);
    return 0;
}
