#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <ctime>
#include <vector>
#include <unordered_map>
#include <cassert>
#include <set>

#define N 10
#define NUM_ITERATIONS 10

// Find pairs in an array that sum to a given value

void printArr(int arr[N]) {
    for (int i = 0; i < N; ++i) {
        std::cout << std::setw(5) << "[" << i << "]";
    }
    std::cout << std::endl;
    for (int i = 0; i < N; ++i) {
        std::cout << std::setw(7) << arr[i];
    }
    std::cout << std::endl;
}

void printPairsThatSumTo(int arr[N], int sum) {
    // For sum, make a (vector of vectors) representing values 0 to the sum
    std::vector<std::vector<int>> indicesToValues (sum + 1);
    // Iterate through arr, and add indices of values matching indices
    for (int i = 0; i < N; ++i) {
        if (arr[i] < sum + 1) {
            indicesToValues[arr[i]].push_back(i);
        }
    }
    // Now we have a vector where each element contains indices in arr of that
    // index as their value
    std::set<std::pair<int, int>> results; // Avoid dupes from switching indices
    for (auto it1 = indicesToValues.begin(); it1 != indicesToValues.end();
         ++it1) {
        auto index = it1 - indicesToValues.begin();
        for (auto it2 = it1->begin(); it2 != it1->end(); ++it2) {
            // We are trying to find target in arr[*it2] + target = sum
            auto target = indicesToValues[sum - index];
            for (auto it3 = target.begin(); it3 != target.end(); ++it3) {
                if (*it3 != *it2) { // Pair cannot be a dupe of itself
                    // Avoid dupes by storing only a sorted version
                    std::pair<int, int> p;
                    p.first = *it3 < *it2 ? *it3 : *it2;
                    p.second = *it3 < *it2 ? *it2 : *it3;
                    results.insert(p);
                }
            }
        }
    }
    // Print only unique results
    for (auto it = results.begin(); it != results.end(); ++it) {
        std::cout << "(" << it->first << ", " << it->second << ") = "
                  << arr[it->first] << " + " << arr[it->second] << " = "
                  << arr[it->first] + arr[it->second] << std::endl;
        assert(sum == arr[it->first] + arr[it->second]);
    }
}

std::vector<int> twoSum(const std::vector<int> & nums, int target) {
    std::unordered_map<int, std::set<int>> numberToIndices;
    for (auto it = nums.begin(); it != nums.end(); ++it) {
        numberToIndices[*it].insert(it - nums.begin());
    }
    for (auto it = numberToIndices.begin(); it != numberToIndices.end(); ++it) {
        int number = it->first;
        auto seek = numberToIndices.find(target - number);
        if (seek == numberToIndices.end()) { continue; }
        auto i1 = it->second.begin();
        auto i2 = seek->second.begin();
        while (*i1 == *i2) {
            if (next(i1) != it->second.end()) { ++i1; }
            else if (next(i2) != seek->second.end()) { ++i2; }
            else {
                break;
            }
        }
        if (*i1 == *i2) { continue; }
        if (*i1 < *i2) {
            return std::vector<int> {*i1 + 1, *i2 + 1};
        } else {
            return std::vector<int> {*i2 + 1, *i1 + 1};
        }
    }
    return std::vector<int>();
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        int arr[N];
        for (int i = 0; i < N; ++i) {
            arr[i] = rand() % 10 + 1;
        }
        int sum = rand() % 19 + 2;
        std::cout << "Sum: " << sum << std::endl;
        printArr(arr);
        printPairsThatSumTo(arr, sum);
        std::cout << std::endl;
    }
    auto result = twoSum(std::vector<int> {1, 2, 3, 4, 5}, 3);
    assert(result[0] = 1);
    assert(result[1] = 2);
    return 0;
}
