#include <iostream>
#include <fstream>
#include <cassert>
#include <string>
#include <unordered_map>
#include <vector>
#include <climits>
#include <cmath>
#include <cassert>

// Given a file of words and 2 words, find the shortest distance between them

int min(int a, int b) {
    return a < b ? a : b;
}

void printData(const std::unordered_map<std::string, std::vector<int>> & data) {
    for (auto it = data.begin(); it != data.end(); ++it) {
        std::cout << it->first << std::endl;
        std::cout << "\t";
        for (auto wordN = it->second.begin(); wordN != it->second.end();
             ++wordN) {
            std::cout << *wordN << " ";
        }
        std::cout << std::endl;
    }
}

int findClosestToAux(std::vector<int>::const_iterator front,
                     std::vector<int>::const_iterator back, int n1) {
    assert(back - front >= 0);
    if (front == back) {
        return *front;
    } else if (std::next(front) == back) {
        auto diff1 = abs(*front - n1);
        auto diff2 = abs(*back - n1);
        return diff1 < diff2 ? *front : *back;
    }
    auto midIt = std::next(front, std::distance(front, back) / 2);
    assert(midIt != front);
    assert(midIt != back);
    auto diff1 = abs(n1 - *std::prev(midIt));
    auto diff2 = abs(n1 - *std::next(midIt));
    auto diff3 = abs(n1 - *midIt);
    if (diff3 < diff1 && diff3 < diff2) {
        return *midIt;
    }
    if (diff1 < diff2) {
        return findClosestToAux(front, std::prev(midIt), n1);
    } else {
        return findClosestToAux(std::next(midIt), back, n1);
    }
}

// Find the item in nums2 closest in value to n1 (a modified binary search)
int findClosestTo(int n1, const std::vector<int> & nums2) {
    assert(!nums2.empty());
    return findClosestToAux(nums2.begin(), std::prev(nums2.end()),  n1);
}

// Used with vectors of line numbers as arguments (finds the least difference
// between a pair of elements (one from each vector))
int findLeastDifferenceBetweenElementsFrom(const std::vector<int> & nums1,
                                           const std::vector<int> & nums2) {
    auto leastDiff = INT_MAX;
    for (auto nums1it = nums1.begin(); nums1it != nums1.end(); ++nums1it) {
        int closestInNums2 = findClosestTo(*nums1it, nums2);
        auto diff = abs(closestInNums2 - *nums1it);
        if (diff < leastDiff) {
            leastDiff = diff;
        }
    }
    return leastDiff;
}

int findDistanceBetween(const std::string & s1, const std::string & s2,
                        std::unordered_map<std::string,
                                           std::vector<int>> & data) {
    if (s1.compare(s2) == 0) { return 0; }
    if (data.find(s1) == data.end()) { return -1; }
    if (data.find(s2) == data.end()) { return -1; }
    auto lineNums1 = data[s1];
    auto lineNums2 = data[s2];
    return findLeastDifferenceBetweenElementsFrom(lineNums1, lineNums2);
}

int testFindDistanceBetween(const std::string & s1, const std::string & s2,
                            const std::vector<std::string> & fileAsAnArray) {
    if (s1.compare(s2) == 0) { return 0; }
    int minDistance = INT_MAX;
    bool found = false;
    for (auto it = fileAsAnArray.begin(); it != fileAsAnArray.end(); ++it) {
        if (it->compare(s1) == 0) {
            found = true;
            auto itBack = std::vector<std::string>::const_reverse_iterator(it);
            --itBack; // When converted, it points to predecessor of it
            auto beginR = itBack;
            assert(beginR->compare(*it) == 0);
            auto itForth = it;
            auto distance = INT_MAX;
            while (itBack != fileAsAnArray.rend()) {
                if (itBack->compare(s2) == 0) {
                    distance = abs(std::distance(itBack, beginR));
                    break;
                }
                ++itBack;
            }
            while (itForth != fileAsAnArray.end()) {
                if (itForth->compare(s2) == 0) {
                    distance = min(distance, abs(std::distance(it, itForth)));
                    break;
                }
                ++itForth;
            }
            minDistance = min(distance, minDistance);
        }
    }
    if (!found) { return -1; }
    return minDistance;
}

int main() {
    std::unordered_map<std::string, std::vector<int>> data;
    std::vector<std::string> fileAsAnArray; // For testing
    // Read from file and build a mapping of words to sets of line numbers
    std::ifstream fileIn;
    fileIn.open("input.txt");
    std::string currWord;
    int i = 0;
    while (fileIn >> currWord) {
        data[currWord].push_back(i++);
        fileAsAnArray.push_back(currWord); // For testing
    }
    fileIn.close();
    printData(data);
    // For each word, test how close it is to every other word
    for (auto it1 = data.begin(); it1 != data.end(); ++it1) {
        for (auto it2 = data.begin(); it2 != data.end(); ++it2) {
            std::cout << "Distance between " << it1->first << " and " << it2->first << " is ";
            auto d1 = findDistanceBetween(it1->first, it2->first, data);
            auto d2 = testFindDistanceBetween(it1->first, it2->first,
                                              fileAsAnArray);
            if (d1 != d2) { assert(0); }
            std::cout << d1 << std::endl;
        }
    }
    return 0;
}
