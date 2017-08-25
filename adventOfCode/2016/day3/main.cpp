#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <algorithm>
#include <cassert>

/*
From http://adventofcode.com/2016/day/3

--- Day 3: Squares With Three Sides ---

Now that you can think clearly, you move deeper into the labyrinth of hallways
and office furniture that makes up this part of Easter Bunny HQ. This must be a
graphic design department; the walls are covered in specifications for
triangles.

Or are they?

The design document gives the side lengths of each triangle it describes,
but... 5 10 25? Some of these aren't triangles. You can't help but mark the
impossible ones.

In a valid triangle, the sum of any two sides must be larger than the remaining
side. For example, the "triangle" given above is impossible, because 5 + 10 is
not larger than 25.

In your puzzle input, how many of the listed triangles are possible?--- Day 3:
Squares With Three Sides ---

Now that you can think clearly, you move deeper into the labyrinth of hallways
and office furniture that makes up this part of Easter Bunny HQ. This must be a
graphic design department; the walls are covered in specifications for
triangles.

Or are they?

The design document gives the side lengths of each triangle it describes,
but... 5 10 25? Some of these aren't triangles. You can't help but mark the
impossible ones.

In a valid triangle, the sum of any two sides must be larger than the remaining
side. For example, the "triangle" given above is impossible, because 5 + 10 is
not larger than 25.

In your puzzle input, how many of the listed triangles are possible?

--- Part Two ---

Now that you've helpfully marked up their design documents, it occurs to you
that triangles are specified in groups of three vertically. Each set of three
numbers in a column specifies a triangle. Rows are unrelated.

For example, given the following specification, numbers with the same hundreds
digit would be part of the same triangle:

101 301 501 102 302 502 103 303 503 201 401 601 202 402 602 203 403 603 In your
puzzle input, and instead reading by columns, how many of the listed triangles
are possible?
*/

int part1(const char * filename) {
    std::ifstream inputFile(filename);
    if (!inputFile.is_open()) {
        std::cerr << filename << " cannot be opened.\n";
        return 1;
    }
    std::string currentLine;
    int numberOfValidTriangles = 0;
    while (std::getline(inputFile, currentLine)) {
        std::stringstream ss(currentLine);
        std::array<int, 3> lengths;
        if (!(ss >> lengths[0]) || !(ss >> lengths[1]) || !(ss >> lengths[2])) {
            std::cerr << "Could not read triangle length.\n";
            return 1;
        }
        std::sort(lengths.begin(), lengths.end());
        if (lengths[0] + lengths[1] > lengths[2]) {
            ++numberOfValidTriangles;
        }
    }
    assert(numberOfValidTriangles == 862);
    std::cout << "Part 1: " << numberOfValidTriangles << std::endl;
    return 0;
}

int part2(const char * filename) {
    std::ifstream inputFile(filename);
    if (!inputFile.is_open()) {
        std::cerr << filename << " cannot be opened.\n";
        return 1;
    }
    std::array<std::string, 3> lineTriplet;
    int numberOfValidTriangles = 0;
    while (std::getline(inputFile, lineTriplet[0]) &&
           std::getline(inputFile, lineTriplet[1]) &&
           std::getline(inputFile, lineTriplet[2])) {
        assert(lineTriplet[0].size() == lineTriplet[1].size());
        assert(lineTriplet[0].size() == lineTriplet[2].size());
        std::stringstream ss0(lineTriplet[0]);
        std::stringstream ss1(lineTriplet[1]);
        std::stringstream ss2(lineTriplet[2]);
        std::array<int, 3> lengths;
        while ((ss0 >> lengths[0]) &&
               (ss1 >> lengths[1]) &&
               (ss2 >> lengths[2])) {
            std::sort(lengths.begin(), lengths.end());
            if (lengths[0] + lengths[1] > lengths[2]) {
                ++numberOfValidTriangles;
            }
        }
    }
    assert(numberOfValidTriangles == 1577);
    std::cout << "Part 2: " << numberOfValidTriangles << std::endl;
    return 0;
}

int main(int argc, char * argv[]) {
    if (2 != argc) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    part1(argv[1]);
    part2(argv[1]);
}
