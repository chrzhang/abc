#include <iostream>
#include <cassert>
#include <vector>
#include <algorithm>

// Find the length of the longest strictly increasing sub-array

int lenOfLongestIncreasingSubarray(const std::vector<int> & v) {
    if (v.empty()) { return 0; }
    int longestEverFound = 1;
    int prev = 1;
    for (size_t i = 1; i < v.size(); ++i) {
        if (v[i] > v[i - 1]) {
            prev += 1;
            longestEverFound = std::max(longestEverFound, prev);
        } else {
            prev = 1;
        }
    }
    return longestEverFound;
}

int main() {
    assert(lenOfLongestIncreasingSubarray(
               std::vector<int>({-4, 5, -2, 0 , 4 , 5 , 9 ,9})) == 5);
    assert(lenOfLongestIncreasingSubarray(
               std::vector<int>({1, 0})) == 1);
    assert(lenOfLongestIncreasingSubarray(
               std::vector<int>({0, 1})) == 2);
    return 0;
}
