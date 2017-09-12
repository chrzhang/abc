#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <regex>
#include <string>
#include <cassert>

int toInt(const std::string & someNumber) {
    int result = 0;
    std::stringstream ss(someNumber);
    if (!(ss >> result)) {
        std::cerr << "Cannot convert " << someNumber << " to int\n";
        abort();
    }
    return result;
}

struct Node {

    int d_size;
    int d_usedSize;
    int d_available;
    int d_usePercentage;

    Node(const int sizeOfNode, const int usedSize, const int availableSize,
         const int usePercentage)
    : d_size(sizeOfNode), d_usedSize(usedSize), d_available(availableSize),
      d_usePercentage(usePercentage)
    {}

};

bool isViable(const Node & node1, const Node & node2) {
    if (&node1 == &node2) {
        std::cerr << "A node cannot be viable with itself." << std::endl;
        abort();
    }
    return (node1.d_usedSize != 0 && node1.d_usedSize <= node2.d_available) ||
           (node2.d_usedSize != 0 && node2.d_usedSize <= node1.d_available);
}

size_t getGridCount(const std::vector<std::vector<Node>> & grid) {
    size_t counter = 0;
    for (const auto & row : grid) {
        counter += row.size();
    }
    return counter;
}

int part1(const std::vector<std::vector<Node>> & grid) {
    int viableCounter = 0;
    // Consider all pairings of nodes
    const size_t totalNodeCount = getGridCount(grid);
    const size_t nodesPerRow = grid[0].size();
    std::vector<bool> chosen(totalNodeCount, false);
    chosen[totalNodeCount - 1] = chosen[totalNodeCount - 2] = true;
    do {
        int indexOfNodeA = -1;
        int indexOfNodeB = -1;
        for (size_t ii = 0; ii < chosen.size(); ++ii) {
            if (chosen[ii]) {
                if (indexOfNodeA == -1) {
                    indexOfNodeA = ii;
                } else if (indexOfNodeB == -1) {
                    indexOfNodeB = ii;
                    break;
                }
            }
        }
        assert(indexOfNodeA != -1 && indexOfNodeB != -1
               && indexOfNodeA != indexOfNodeB);
        const int nodeARow = indexOfNodeA / nodesPerRow;
        const int nodeACol = indexOfNodeA  % nodesPerRow;
        const int nodeBRow = indexOfNodeB / nodesPerRow;
        const int nodeBCol = indexOfNodeB  % nodesPerRow;
        if (isViable(grid[nodeARow][nodeACol],
                     grid[nodeBRow][nodeBCol])) {
            ++viableCounter;
        }
    } while (std::next_permutation(chosen.begin(), chosen.end()));
    return viableCounter;
}

int main(int argc, char * argv[]) {
    if (2 != argc) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::ifstream inputFile(argv[1]);
    if (!inputFile.is_open()) {
        std::cerr << "Cannot open " << argv[1] << std::endl;
        return 1;
    }
    std::string currentLine;
    std::getline(inputFile, currentLine);
    assert(currentLine == "root@ebhq-gridcenter# df -h");
    std::getline(inputFile, currentLine);
    assert(currentLine == "Filesystem              Size  Used  Avail  Use%");
    std::regex lineRegex("^/dev/grid/node\\-x([0-9]+)\\-y([0-9]+)[ ]+([0-9]+)T[ ]+([0-9]+)T[ ]+([0-9]+)T[ ]+([0-9]+)%$");
    std::vector<std::vector<Node>> grid;
    while (std::getline(inputFile, currentLine)) {
        std::smatch result;
        if (!std::regex_search(currentLine, result, lineRegex)) {
            std::cerr << "Line is malformed: " << currentLine << std::endl;
            abort();
        }
        assert(result.size() == 7);
        const int & xNumber = toInt(result[1]);
        if (xNumber >= (int) grid.size()) {
            grid.push_back({});
        }
        assert(xNumber < (int) grid.size());
        const int & yNumber = toInt(result[2]);
        assert(yNumber == (int) grid[xNumber].size());
        const int & sizeOfNode = toInt(result[3]);
        const int & usedSizeOfNode = toInt(result[4]);
        const int & availableSizeOfNode = toInt(result[5]);
        const int & usePercentageOfNode = toInt(result[6]);
        grid[xNumber].push_back(Node(sizeOfNode, usedSizeOfNode, availableSizeOfNode, usePercentageOfNode));
    }
    const auto & part1Result = part1(grid);
    assert(955 == part1Result);
    std::cout << "Part 1: " << part1Result << std::endl;
    return 0;
}
