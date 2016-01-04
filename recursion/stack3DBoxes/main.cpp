#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <ctime>
#include <vector>
#include <assert.h>
#include <unordered_map> // For DP
#include <algorithm> // For isSorted

#define NUM_BOXES 7

// Given 3D boxes that can only be stacked on bigger boxes, find the highest
// tower of boxes possible

struct Box {
    size_t height, width, depth;
    Box(size_t h, size_t w, size_t d) : height(h), width(w), depth(d) {}
};

bool operator<(const Box & b1, const Box & b2) {
    return (b1.height < b2.height &&
            b1.width < b2.width &&
            b1.depth < b2.depth);
}

std::ostream & operator<<(std::ostream & os, const Box & b) {
    os << "(" << "h" << std::setw(3) << b.height << ", "
              << "w" << std::setw(3) << b.width << ", "
              << "d" << std::setw(3) << b.depth << " )";
    return os;
}

size_t height(const std::vector<int> & tower,
              const std::vector<Box> & boxes) {
    size_t acc = 0;
    for (auto it = tower.begin(); it != tower.end(); ++it) {
        acc += boxes[*it].height;
    }
    return acc;
}

std::vector<int> findHighestTower(int baseBoxIndex,
                                  const std::vector<Box> & boxes,
                                  std::unordered_map<int, std::vector<int>> &
                                    table) {
    // Dynamic programming (look for and store calculations)
    auto searchForPreviousCalculations = table.find(baseBoxIndex);
    if (searchForPreviousCalculations != table.end()) { // Did calc before
        return searchForPreviousCalculations->second;
    }
    std::vector<int> tower, result;
    tower.push_back(baseBoxIndex);
    result.push_back(baseBoxIndex);
    size_t highestHeight = 0;
    for (auto eachBox = boxes.begin(); eachBox != boxes.end(); ++eachBox) {
        // If it can fit above, push the highest sub-tower possible
        if (*eachBox < boxes[baseBoxIndex]) {
            auto copyOfTower = tower;
            auto highestSubTower = findHighestTower(eachBox - boxes.begin(),
                                                    boxes, table);
            copyOfTower.insert(copyOfTower.end(), highestSubTower.begin(),
                         highestSubTower.end());
            auto heightOfCopyOfTower = height(copyOfTower, boxes);
            if (heightOfCopyOfTower > highestHeight) {
                result = copyOfTower;
                highestHeight = heightOfCopyOfTower;
            }
        }
    }
    table[baseBoxIndex] = result;
    return result;
}

std::vector<int> findHighestTower(const std::vector<Box> & boxes) {
    // Return an ordered vector of indices symbolizing the tower
    std::vector<int> tower;
    std::unordered_map<int, std::vector<int>> table;
    size_t highestHeight = 0;
    for (auto eachBox = boxes.begin(); eachBox != boxes.end(); ++eachBox) {
        auto highestTowerWithBase = findHighestTower(eachBox - boxes.begin(),
                                                     boxes, table);
        auto heightOfTower = height(highestTowerWithBase, boxes);
        if (heightOfTower > highestHeight) {
            highestHeight = heightOfTower;
            tower = highestTowerWithBase;
        }
    }
    return tower;
}

int main() {
    srand(time(0));
    std::vector<Box> boxes;
    for (int i = 0; i < NUM_BOXES; ++i) {
        boxes.push_back(Box(rand() % 99 + 1, rand() % 99 + 1, rand() % 99 + 1));
    }
    for (auto it = boxes.begin(); it != boxes.end(); ++it) {
        std::cout << "box " << it - boxes.begin() << ": " << *it << "\n";
    }
    auto tower = findHighestTower(boxes);
    // Display result
    std::cout << "bottom <- ";
    for (auto it = tower.begin(); it != tower.end(); ++it) {
        std::cout << "Box " << *it << " ";
    }
    std::cout << " Height: " << height(tower, boxes) << std::endl;
    std::vector<int> heights, widths, depths;
    for (auto it = tower.begin(); it != tower.end(); ++it) {
        heights.push_back(boxes[*it].height);
        widths.push_back(boxes[*it].width);
        depths.push_back(boxes[*it].depth);
    }
    assert(std::is_sorted(heights.rbegin(), heights.rend()));
    assert(std::is_sorted(widths.rbegin(), widths.rend()));
    assert(std::is_sorted(depths.rbegin(), depths.rend()));
    return 0;
}
