#include <iostream>
#include <vector>
#include <algorithm>

// Find the next lexigoraphic sequence that is a permutation of the current #

void nextPermutation(std::vector<int> & nums) {
    if (nums.size() < 2) { return; }
    // Idea is to increase the current number as little as possible
    // Find the lowest magnitude number that can be swapped with another
    // Start from end and go left in pairs, stop if left # < right # (can swap)
    auto left = std::next(nums.rbegin());
    auto right = nums.rbegin();
    bool found = false; // In case permutation is already maxed (e.g. 3, 2, 1)
    while (left != nums.rend()) {
        if (*left < *right) {
            found = true;
            // Start from the right, find first # > *left
            for (auto it = nums.rbegin(); it != nums.rend(); ++it) {
                if (*it > *left) {
                    // Swap so that the number increases as little as possible
                    std::swap(*it, *left);
                    break;
                }
            }
            // Reverse the subvector [rbegin, right] or [rbegin, left)
            // The number is already bigger because a digit in an order of
            // magnitude has increased (sort the smaller orders to minimize
            // the increase)
            std::reverse(nums.rbegin(), left);
            break;
        }
        ++left;
        ++right;
    }
    if (!found) { std::sort(nums.begin(), nums.end()); }
}

unsigned long fact(unsigned i) {
    return (i == 1 || i == 0) ? 1 : i * fact(i - 1);
}

int main() {
    std::vector<int> v = {1, 2, 3};
    for (unsigned long i = 0; i <= fact(v.size()); ++i) {
        for (auto n : v) {
            std::cout << n << " ";
        }
        std::cout << std::endl;
        nextPermutation(v);
    }
    return 0;
}
