#include <iostream>
#include <vector>
#include <algorithm>
#include <random>
#include <cassert>

#define N 10

// Every element in an array has a duplicate except one that needs to be found

void printVec(const std::vector<int> & v) {
    for (auto it = v.begin(); it != v.end(); ++it) {
        std::cout << *it << " ";
    }
    std::cout << std::endl;
}

int findSingleNumber(const std::vector<int> & v) {
    // XOR-ing two equal numbers yields 0
    // XOR is commutative and associative so order does not matter
    // Every duplicate will cancel out, leaving only the lonely single number
    int x = 0;
    for (auto it = v.begin(); it != v.end(); ++it) {
        x ^= *it;
    }
    return x;
}

int main() {
    for (int iteration = 0; iteration < 100000; ++iteration) {
        std::vector<int> v;
        for (int i = 0; i < N; ++i) {
            v.push_back(i);
            v.push_back(i);
        }
        v.push_back(N);
        std::random_device rd;
        std::mt19937 g(rd());
        std::shuffle(v.begin(), v.end(), g);
        assert(N == findSingleNumber(v));
    }
    return 0;
}
