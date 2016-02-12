#include <iostream>
#include <iomanip>
#include <vector>
#include <cassert>
#include <climits>

// Given an array of wall heights, find a pair of walls forming the biggest box

template<typename T>
T min(T a, T b) {
    return a > b ? b : a;
}

template<typename T>
T max(T a, T b) {
    return a > b ? a : b;
}

void printWalls(const std::vector<unsigned> & v) {
    assert(v.size() >= 2);
    size_t max = 0;
    for (auto wallHeight : v) {
        if (wallHeight > max) {
            max = wallHeight;
        }
    }
    for (auto currentHeight = max; currentHeight > 0; --currentHeight) {
        std::cout << std::setw(3) << currentHeight << " ";
        for (auto wallHeight : v) {
            if (wallHeight >= currentHeight) {
                std::cout << "|  ";
            } else {
                std::cout << "   ";
            }
        }
        std::cout << std::endl;
    }
}

void findPairOfWallsForTheBiggestBox(const std::vector<unsigned> & v) {
    assert(v.size() >= 2);
    size_t biggestVolSoFar = 0;
    size_t left, right;
    left = right = 0;
    for (size_t currentWallIndex = 0; currentWallIndex < v.size();
         ++currentWallIndex) {
        size_t potentialRightWallIndex;
        if (v[currentWallIndex] != 0) {
            potentialRightWallIndex = currentWallIndex + 1 +
                                      biggestVolSoFar / v[currentWallIndex];
        } else {
            potentialRightWallIndex = currentWallIndex + 1;
        }
        for (; potentialRightWallIndex < v.size(); ++potentialRightWallIndex) {
            assert(potentialRightWallIndex > currentWallIndex);
            size_t volume = min(v[currentWallIndex],
                                v[potentialRightWallIndex]) *
                                (potentialRightWallIndex - currentWallIndex);
            if (volume > biggestVolSoFar) {
                biggestVolSoFar = volume;
                left = currentWallIndex;
                right = potentialRightWallIndex;
            }
        }
    }
    std::cout << "Left wall at index: " << left << " with height " << v[left]
              << " and right wall at index: " << right << " with height "
              << v[right] << " creates volume " << biggestVolSoFar << std::endl;
}

// A faster, cleaner version than the previous
size_t findVolumeOfBiggestBox(const std::vector<unsigned> & v) {
    // Start at either end of the walls (at the longest width)
    // Close in using two pointers, advancing the shorter wall (we are aiming
    // to achieve a greater height as we shrink width, hoping that the increase
    // in height may be enough to overcome the volume lost by decreasing width)
    assert(v.size() >= 2);
    size_t biggestVolSoFar = 0;
    size_t left = 0;
    size_t right = v.size() - 1;
    while (left < right) {
        biggestVolSoFar = max(biggestVolSoFar, min(v[left], v[right]) *
                                               (right - left));
        if (v[left] < v[right]) {
            ++left;
        } else {
            --right;
        }
    }
    return biggestVolSoFar;
}

int main() {
    std::vector<unsigned> wallHeights = { 5, 3, 2, 1, 4, 7, 3, 8 };
    printWalls(wallHeights);
    findPairOfWallsForTheBiggestBox(wallHeights);
    assert(findVolumeOfBiggestBox(wallHeights) == 35);
    return 0;
}
