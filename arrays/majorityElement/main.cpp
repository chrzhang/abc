#include <iostream>
#include <vector>
#include <unordered_map>
#include <cassert>

// Find the majority element (occurs more than N / 2 times)

int majorityElement(const std::vector<int> & nums) {
    std::unordered_map<int, size_t> counts;
    for (auto n : nums) {
        auto seek = counts.find(n);
        if (seek == counts.end()) {
            counts[n] = 1;
            if (counts[n] > nums.size() / 2) {
                return n;
            }
        } else {
            ++(seek->second);
            if (seek->second > nums.size() / 2) {
                return seek->first;
            }
        }
    }
    printf("There is no majority element.\n");
    assert(0);
}

int main() {
    std::vector<int> v = {1, 2, 2};
    assert(majorityElement(v) == 2);
    v = {3, 2, 3};
    assert(majorityElement(v) == 3);
    v = {1, 2, 3, 4, 5, 5, 5, 5, 5};
    assert(majorityElement(v) == 5);
    return 0;
}
