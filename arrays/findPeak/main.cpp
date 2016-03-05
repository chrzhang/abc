#include <iostream>
#include <vector>
#include <cassert>

// Find a peak (number greater than its neighbors assuming past ends are -INF)

int findPeakElement(const std::vector<int> & nums) {
    if (nums.size() == 1) { return 0; }
    for (auto it = nums.begin(); it != nums.end(); ++it) {
        // Check left
        auto leftVal = it == nums.begin() ? INT_MIN : *prev(it);
        auto rightVal = next(it) == nums.end() ? INT_MIN : *next(it);
        if (*it > leftVal && *it > rightVal) {
            return std::distance(nums.begin(), it);
        }
    }
    return -1;
}

int main() {
    std::vector<int> v;
    v = {1, 2, 3};
    assert(findPeakElement(v) == 2);
    v = {5, 3, 7};
    assert(findPeakElement(v) == 0);
    v = {9, 3, 2};
    assert(findPeakElement(v) == 0);
    v = {2, 8, 4};
    assert(findPeakElement(v) == 1);
    return 0;
}
