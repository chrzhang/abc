#include <iostream>
#include <fstream>
#include <array>
#include <algorithm>
#include <climits>

/*
From http://adventofcode.com/2015/day/2

--- Day 2: I Was Told There Would Be No Math ---

The elves are running low on wrapping paper, and so they need to submit an
order for more. They have a list of the dimensions (length l, width w, and
height h) of each present, and only want to order exactly as much as they need.

Fortunately, every present is a box (a perfect right rectangular prism), which
makes calculating the required wrapping paper for each gift a little easier:
find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l. The elves
also need a little extra paper for each present: the area of the smallest side.

For example:

A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square feet of
wrapping paper plus 6 square feet of slack, for a total of 58 square feet.  A
present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42 square feet of
wrapping paper plus 1 square foot of slack, for a total of 43 square feet.  All
numbers in the elves' list are in feet. How many total square feet of wrapping
paper should they order?

--- Part Two ---

The elves are also running low on ribbon. Ribbon is all the same width, so they
only have to worry about the length they need to order, which they would again
like to be exact.

The ribbon required to wrap a present is the shortest distance around its
sides, or the smallest perimeter of any one face. Each present also requires a
bow made out of ribbon as well; the feet of ribbon required for the perfect bow
is equal to the cubic feet of volume of the present. Don't ask how they tie the
bow, though; they'll never tell.

For example:

A present with dimensions 2x3x4 requires 2+2+3+3 = 10 feet of ribbon to wrap
the present plus 2*3*4 = 24 feet of ribbon for the bow, for a total of 34 feet.
A present with dimensions 1x1x10 requires 1+1+1+1 = 4 feet of ribbon to wrap
the present plus 1*1*10 = 10 feet of ribbon for the bow, for a total of 14
feet.  How many total feet of ribbon should they order?
*/

void update(unsigned & totalSurfaceArea, unsigned & smallestSideArea,
            unsigned & smallestSidePerimeter, unsigned dim1, unsigned dim2) {
    unsigned currArea = dim1 * dim2;
    smallestSideArea = std::min(smallestSideArea, currArea);
    smallestSidePerimeter = std::min(smallestSidePerimeter, 2 * (dim1 + dim2));
    totalSurfaceArea += 2 * currArea;
}

int main(int argc, char * argv[]) {
    if (argc == 1) {
        std::cout << "Usage: " << argv[0] << " <filename>\n";
        return 0;
    }
    std::ifstream f(argv[1]);
    std::string line;
    const std::string delimiter="x";
    unsigned totalSurfaceArea = 0;
    unsigned totalRibbon = 0;
    while (std::getline(f, line)) {
        std::array<unsigned, 3> dimensions;
        size_t curr_dimension = 0;
        size_t start = 0;
        size_t seek = line.find(delimiter);
        while (seek != std::string::npos) {
            dimensions[curr_dimension++] =
                std::strtoul(line.substr(start, seek - start).c_str(), NULL, 0);
            start = seek + delimiter.length();
            seek = line.find(delimiter, start);
        }
        dimensions[curr_dimension++] =
            std::strtoul(line.substr(start, seek).c_str(), NULL, 0);
        if (curr_dimension != 3) {
            std::cerr << "Incorrect number of dimensions\n";
            return 1;
        }
        unsigned smallestSideArea = UINT_MAX;
        unsigned smallestSidePerimeter = UINT_MAX;
        update(totalSurfaceArea, smallestSideArea, smallestSidePerimeter,
               dimensions[0], dimensions[1]);
        update(totalSurfaceArea, smallestSideArea, smallestSidePerimeter,
               dimensions[0], dimensions[2]);
        update(totalSurfaceArea, smallestSideArea, smallestSidePerimeter,
               dimensions[1], dimensions[2]);
        totalSurfaceArea += smallestSideArea;
        totalRibbon += smallestSidePerimeter;
        totalRibbon += dimensions[0] * dimensions[1] * dimensions[2];
    }
    std::cout << "Total wrapping paper: " << totalSurfaceArea
              << " square feet\n";
    std::cout << "Total ribbon: " << totalRibbon << " feet.\n";
    return 0;
}
