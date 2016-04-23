#include <iostream>
#include <cassert>
#include <vector>
#include <algorithm>

// Find the length of the longest strictly increasing sub-array

int lenOfLongestIncreasingSubarray(const std::vector<int> & v) {
    if (v.empty()) { return 0; }
    std::vector<int> lensOfLongestIncSubarraysEndingAtEachIndex(v.size(), 1);
    for (size_t i = 1; i < v.size(); ++i) {
        if (v[i] > v[i - 1]) {
            lensOfLongestIncSubarraysEndingAtEachIndex[i] =
                lensOfLongestIncSubarraysEndingAtEachIndex[i - 1] + 1;
        }
    }
    return *std::max_element(lensOfLongestIncSubarraysEndingAtEachIndex.begin(),
                             lensOfLongestIncSubarraysEndingAtEachIndex.end());
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
