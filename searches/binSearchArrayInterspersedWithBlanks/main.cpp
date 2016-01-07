#include <iostream>
#include <string>
#include <cstdlib>
#include <ctime>
#include <assert.h>
#include <vector>

#define SIZE 25 // Max 25
#define NUM_ITERATIONS 1000000

// Search for a string in a sorted array of strings interspersed with spaces

std::string genRandString() {
    std::string s;
    if (rand() % 2 > 0) {
        for (int i = 0; i < 3; ++i) {
            s.push_back('a' + rand() % 26);
        }
    }
    return s;
}

void printStrings(std::string allStrings[SIZE]) {
    for (int i = 0; i < SIZE; ++i) {
        std::cout << "[" << i << "]: " << allStrings[i] << " ";
    }
    std::cout << std::endl;
}

int binSearchAux(int begin, int end, const std::string & s,
                 std::string allStrings[SIZE]) {
    if (begin > end) { return -1; }
    auto mid = (begin + end) / 2;
    if (0 == allStrings[mid].compare(s)) {
        return mid;
    }
    if (allStrings[mid].empty()) {
        // Find the closest non-empty spot to use as mid
        auto left = mid - 1;
        auto right = mid + 1;
        while (left >= begin && allStrings[left].empty() &&
               right <= end && allStrings[right].empty()) {
            --left;
            ++right;
        }
        while (left < begin && right <= end && allStrings[right].empty()) {
            ++right;
        }
        while (right > end && left >= begin && allStrings[left].empty()) {
            --left;
        }
        if (!allStrings[left].empty()) {
            mid = left;
        }
        if (!allStrings[right].empty()) {
            mid = right;
        }
    }
    if (0 == allStrings[mid].compare(s)) {
        return mid;
    } else if (allStrings[mid].compare(s) > 0) {
        return binSearchAux(begin, mid - 1, s, allStrings);
    } else {
        return binSearchAux(mid + 1, end, s, allStrings);
    }
}

int binSearch(const std::string & s, std::string allStrings[SIZE]) {
    return binSearchAux(0, SIZE - 1, s, allStrings);
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        // Build sorted strings
        std::string allStrings[SIZE];
        std::vector<std::string> nonEmptyStrings; // For testing
        for (int i = 0; i < SIZE; ++i) {
            auto r = genRandString();
            if (!r.empty()) {
                allStrings[i] = std::string(1, 'a' + i) + r;
                nonEmptyStrings.push_back(allStrings[i]);
            }
        }
        //printStrings(allStrings);
        for (auto it = nonEmptyStrings.begin(); it != nonEmptyStrings.end();
             ++it) {
            assert(allStrings[binSearch(*it, allStrings)].compare(*it) == 0);
        }
    }
    return 0;
}
