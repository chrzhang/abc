#include <iostream>
#include <string>
#include <cstdlib>
#include <ctime>
#include <vector>
#include <assert.h>

#define SIZE 10

// Sort an array of strings interspersed with spaces

std::string genRandomString() {
    std::string s;
    if (rand() % 4 > 1) {
        return s;
    }
    for (int i = 0; i < 3; ++i) {
        s.push_back('a' + rand() % 26);
    }
    return s;
}

void printAllStrings(std::string allStrings[SIZE]) {
    for (int i = 0; i < SIZE; ++i) {
        std::cout << "[" << i << "]: " << allStrings[i] << " ";
    }
    std::cout << std::endl;
}

void printIndices(const std::vector<size_t> & v) {
    for (auto it = v.begin(); it != v.end(); ++it) {
        std::cout << *it << " ";
    }
    std::cout << std::endl;
}

bool onLeft(size_t walker, size_t pivotIndex) {
    return walker < pivotIndex;
}

bool onRight(size_t walker, size_t pivotIndex) {
    return walker > pivotIndex;
}

void swap(size_t & e1, size_t & e2) {
    auto temp = e1;
    e1 = e2;
    e2 = temp;
}

bool isLessThanOrEqual(size_t i1, size_t i2, std::string allStrings[SIZE]) {
    return allStrings[i1].compare(allStrings[i2]) <= 0;
}

bool isGreaterThan(size_t i1, size_t i2, std::string allStrings[SIZE]) {
    return allStrings[i1].compare(allStrings[i2]) > 0;
}

void partition(size_t & pivotIndex, size_t begin, size_t end,
               std::vector<size_t> & v, std::string allStrings[SIZE]) {
    auto pivotVal = v[pivotIndex];
    // Walk through begin to end
    size_t walker = begin;
    while (walker <= end) {
        if (onLeft(walker, pivotIndex)) {
            // Left is where values smaller than pivot are supposed to be
            if (isGreaterThan(v[walker], pivotVal, allStrings)) {
                swap(v[walker], v[pivotIndex]);
                pivotIndex = walker;
            }
        } else if (onRight(walker, pivotIndex)) {
            // Right is where values greater than pivot are supposed to be
            if (isLessThanOrEqual(v[walker], pivotVal, allStrings)) {
                // Shift everything before walker right
                auto temp = v[walker];
                for (size_t i = walker - 1; i >= begin; --i) {
                    v[i + 1] = v[i];
                    if (i == begin) { break; }
                }
                // Put value of walker into beginning
                v[begin] = temp;
                ++pivotIndex;
            }
        }
        ++walker;
    }
}

void quickSort(size_t begin, size_t end, std::vector<size_t> & v,
               std::string allStrings[SIZE]) {
    if (begin >= end) { return; }
    size_t pivotIndex = begin + rand() % (end - begin + 1);
    assert(pivotIndex >= begin && pivotIndex <= end);
    partition(pivotIndex, begin, end, v, allStrings);
    if (pivotIndex > 0) {
        quickSort(begin, pivotIndex - 1, v, allStrings);
    }
    quickSort(pivotIndex + 1, end, v, allStrings);
}

void sortIndices(const std::vector<size_t> & nonEmptyIndices,
                 std::string allStrings[SIZE]) {
    if (nonEmptyIndices.empty()) { return; }
    auto copyOfNonEmptyIndices = nonEmptyIndices;
    quickSort(0, copyOfNonEmptyIndices.size() - 1, copyOfNonEmptyIndices,
              allStrings);
    assert(copyOfNonEmptyIndices.size() == nonEmptyIndices.size());
    std::string copyOfAllStrings[SIZE];
    for (int i = 0; i < SIZE; ++i) {
        copyOfAllStrings[i] = allStrings[i];
    }
    auto itS = copyOfNonEmptyIndices.begin(); // Sorted according to strings
    auto itN = nonEmptyIndices.begin();
    for (; itS != copyOfNonEmptyIndices.end(); ++itS, ++itN) {
        allStrings[*itN] = copyOfAllStrings[*itS];
    }
}

int main() {
    srand(time(0));
    std::string allStrings[SIZE];
    for (size_t i = 0; i < SIZE; ++i) {
        allStrings[i] = genRandomString();
    }
    printAllStrings(allStrings);
    std::vector<size_t> nonEmptyIndices;
    for (size_t i = 0; i < SIZE; ++i) {
        if (!allStrings[i].empty()) {
            nonEmptyIndices.push_back(i);
        }
    }
    sortIndices(nonEmptyIndices, allStrings);
    std::cout << "Sorting...\n";
    printAllStrings(allStrings);
    return 0;
}
