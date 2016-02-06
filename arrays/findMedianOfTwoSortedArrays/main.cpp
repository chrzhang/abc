#include <iostream>
#include <vector>
#include <algorithm>
#include <cassert>

// Given two sorted vectors, find the median

double findMedianSortedArrays(const std::vector<int> & nums1,
                              const std::vector<int> & nums2) {
    auto totalSize = nums1.size() + nums2.size();
    if (totalSize == 0) { assert(false); }
    if (totalSize == 1) {
        return nums1.empty() ? nums2.front() : nums1.front();
    }
    auto it1 = nums1.begin();
    auto it2 = nums2.begin();
    int target = totalSize / 2;
    int index = 0;
    int current, prev;
    current = prev = 0;
    for (;;) {
        ++index;
        if (it1 != nums1.end() && it2 != nums2.end()) {
            if (*it1 < *it2) {
                prev = *it1;
                ++it1;
                if (it1 != nums1.end()) {
                    current = *it1 < *it2 ? *it1 : *it2;
                }
                else {
                    current = *it2;
                }
            } else {
                prev = *it2;
                ++it2;
                if (it2 != nums2.end()) {
                    current = *it1 < *it2 ? *it1 : *it2;
                }
                else {
                    current = *it1;
                }
            }
        } else if (it1 != nums1.end()) {
            prev = *it1;
            ++it1;
            current = *it1;
        } else if (it2 != nums2.end()) {
            prev = *it2;
            ++it2;
            current = *it2;
        } else {
            assert(false);
        }
        if (index == target) {
            if (totalSize % 2 == 0) {
                return (((double) prev) + current) / 2;
            } else {
                return (double) (current);
            }
        }
    }
}

double mean(int a, int b) {
    return ((double) a + b) / 2;
}

double middleOf(const std::vector<int> & v) {
    if (v.size() == 1) { return (double) v.front(); }
    if (v.size() % 2 == 0) {
        return mean(v[v.size() / 2], v[(v.size() / 2) - 1]);
    } else {
        return (double) v[v.size() / 2];
    }
}

int main() {
    // Use a merge style algorithm by walking through two pointers from
    // beginning to end of each vector in sorted order and stopping once
    // half of the total size has been traversed
    for (int totalSize = 1; totalSize < 100; ++totalSize) {
        std::vector<int> v;
        for (int i = 0; i < totalSize; ++i) {
            v.push_back(rand() % 100);
        }
        std::sort(v.begin(), v.end());
        double trueMedian = middleOf(v);
        std::vector<int> v1, v2;
        for (auto x : v) {
            if (rand() % 2) {
                v1.push_back(x);
            } else {
                v2.push_back(x);
            }
        }
        std::sort(v1.begin(), v1.end());
        std::sort(v2.begin(), v2.end());
        assert(findMedianSortedArrays(v1, v2) == trueMedian);
    }
    return 0;
}
