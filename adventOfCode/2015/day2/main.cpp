#include <iostream>
#include <fstream>
#include <array>
#include <algorithm>
#include <climits>

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
