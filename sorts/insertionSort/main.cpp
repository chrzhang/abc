#include <iostream>
#include <vector>
#include <algorithm> // To baseline results
#include <cstdlib>
#include <ctime>
#include <cassert>

#define N 10
#define NUM_ITERATIONS 1000000

// Perform insertion sort

bool lessThan(int a, int b) {
    return a < b;
}

bool greaterThan(int a, int b) {
    return a > b;
}

template<typename T>
void isort(std::vector<T> & v, bool (*cmp) (int, int)) {
    for (int j = 1; j < v.size(); ++j) {
        auto key = v[j]; // Store value being placed into correct position
        auto i = j - 1; // Re-arrange sorted sublist to make room
        while (i >= 0 && cmp(v[i], key)) {
            v[i + 1] = v[i]; // Shift over to make room
            --i;
        }
        v[i + 1] = key;
    }
}

int main() {
    srand(time(0));
    for (auto iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        std::vector<int> v1, v2;
        for (auto i = 0; i < N; ++i) {
            auto r = rand() % 100;
            v1.push_back(r);
            v2.push_back(r);
        }
        isort(v1, lessThan);
        std::sort(v2.begin(), v2.end());
        std::reverse(v2.begin(), v2.end());
        assert(v1 == v2);
    }
    return 0;
}
