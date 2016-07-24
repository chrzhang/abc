#include <algorithm>
#include <iostream>
#include <vector>
#include <utility>

// Given two sorted arrays, find the k pairs with one element
// from each array that have the smallest sums

struct PairCompare {
    bool operator()(const std::pair<int, int> & p1,
                    const std::pair<int, int> & p2) const {
        return (p1.first + p1.second) < (p2.first + p2.second);
    }
};

std::vector<std::pair<int, int>>
kSmallestPairs(const std::vector<int> & a1,
               const std::vector<int> & a2, size_t k) {
    std::vector<std::pair<int, int>> heap;
    for (size_t i = 0; i < a1.size(); ++i) {
        for (size_t j = 0; j < a2.size(); ++j) {
            if (heap.size() < k) {
                heap.push_back(std::make_pair(a1[i], a2[j]));
                if (heap.size() == k) {
                    std::make_heap(heap.begin(), heap.end(), PairCompare());
                }
            } else if (a1[i] + a2[j] <
                       heap.front().first + heap.front().second) {
                heap[0] = std::make_pair(a1[i], a2[j]);
                std::make_heap(heap.begin(), heap.end(), PairCompare());
            }
        }
    }
    std::sort_heap(heap.begin(), heap.end(), PairCompare());
    return heap;
}

void outputResults(const std::vector<std::pair<int, int>> & result) {
    for (auto p : result) {
        std::cout << "[" << p.first << ", " << p.second << "] ";
    }
    std::cout << "\n";
}

int main() {
    {
        auto result = kSmallestPairs({1, 7, 11}, {2, 4, 6, 20}, 3);
        outputResults(result);
    }
    {
        auto result = kSmallestPairs({1, 1, 2}, {1, 2, 3}, 2);
        outputResults(result);
    }
    {
        auto result = kSmallestPairs({1, 2}, {3}, 3);
        outputResults(result);
    }
    {
        auto result = kSmallestPairs({1, 1, 2}, {1, 2, 3}, 2);
        outputResults(result);
    }
    return 0;
}
