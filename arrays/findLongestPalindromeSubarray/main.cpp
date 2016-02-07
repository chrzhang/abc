#include <iostream>
#include <vector>
#include <cassert>
#include <cstdlib>
#include <ctime>

// Find the longest palindrome subarray

template<typename T>
void print(const std::vector<T> & v) {
    for (auto x : v) {
        std::cout << x << " ";
    }
    std::cout << std::endl;
}

std::pair<std::vector<int>::const_iterator, std::vector<int>::const_iterator>
considerAsCenter(const std::vector<int>::const_iterator & center,
                 const std::vector<int> & v) {
    auto before = center;
    auto after = center;
    while (*before == *after) {
        if (before == v.begin() || std::next(after) == v.end()) {
            return std::make_pair(before, after);
        }
        before = std::prev(before);
        after = std::next(after);
    }
    return std::make_pair(std::next(before), std::prev(after));
}

std::pair<std::vector<int>::const_iterator, std::vector<int>::const_iterator>
considerAsCenterBetween(const std::vector<int>::const_iterator & pre,
                        const std::vector<int>::const_iterator & post,
                         const std::vector<int> & v) {
    auto before = pre;
    auto after = post;
    if (*before != *after) {
        return std::make_pair(v.end(), v.end());
    }
    while (*before == *after) {
        if (before == v.begin() || std::next(after) == v.end()) {
            return std::make_pair(before, after);
        }
        before = std::prev(before);
        after = std::next(after);
    }
    return std::make_pair(std::next(before), std::prev(after));
}

void adjustMaxIfNecessary(std::pair<std::vector<int>::const_iterator,
                                    std::vector<int>::const_iterator> & r,
                          int & longestDistanceFound, int & startIndex,
                          int & endIndex, const std::vector<int> & v) {
    if (r.first == v.end() || r.second == v.end()) { return; }
    auto dist = std::distance(r.first, r.second);
    if (dist > longestDistanceFound) {
        longestDistanceFound = dist;
        startIndex = std::distance(v.begin(), r.first);
        endIndex = std::distance(v.begin(), r.second);
    }
}

std::pair<int, int> findLongestPalindromeSubarray(const std::vector<int> & v) {
    // A palindrome has a recursive stucture (it is made up of two identical
    // characters padding a smaller palindrome, which can be empty)
    // Consider every element and every space between elements as a "smaller"
    // palindrome
    int longestDistanceFound = -1;
    int startIndex, endIndex;
    startIndex = endIndex = -1;
    for (auto it = v.begin(); it != v.end(); ++it) {
        // Consider it as a center
        auto r = considerAsCenter(it, v);
        adjustMaxIfNecessary(r, longestDistanceFound, startIndex, endIndex, v);
        if (std::next(it) != v.end()) {
            // Consider between it and std::next(it) as a center
            auto s = considerAsCenterBetween(it, std::next(it), v);
            adjustMaxIfNecessary(s, longestDistanceFound, startIndex, endIndex, v);
        }
    }
    return std::pair<int, int>(startIndex, endIndex);
}

int main() {
    srand(time(0));
    {
        std::vector<int> v { 1, 4, 1, 4, 1, 2, 5, 2, 3, 4 };
        auto r = findLongestPalindromeSubarray(v);
        assert(r.first == 0 && r.second == 4);
    }
    {
        std::vector<int> v { 3, 3, 2, 2, 3, 5, 3, 3, 1, 2 };
        auto r = findLongestPalindromeSubarray(v);
        assert(r.first == 1 && r.second == 4);
    }
    std::vector<int> v;
    for (int i = 0; i < 10; ++i) {
        v.push_back(rand() % 5 + 1);
    }
    print(v);
    auto r = findLongestPalindromeSubarray(v);
    std::cout << r.first << " to " << r.second
              << " are the indices delimiting the longest palindrome.\n";
    return 0;
}
