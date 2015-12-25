#include <iostream>
#include <ctime>
#include <cstdlib>
#include <string>
#include <limits.h>
#include <bitset>
#include <assert.h>

// Count the # of bits needed to convert integer A to B

size_t countSetBits(const std::string & s) {
    size_t acc = 0;
    for (auto it = s.begin(); it != s.end(); ++it) {
        if (*it == '1') {
            ++acc;
        }
    }
    return acc;
}

size_t countDiffBits(int x, int y) {
    size_t diff = 0;
    auto xs = std::bitset<32>(x).to_string();
    auto ys = std::bitset<32>(y).to_string();
    for (auto itx = xs.begin(), ity = ys.begin(); itx != xs.end()
         && ity != ys.end(); ++itx, ++ity) {
        if (*itx != *ity) {
            ++diff;
        }
    }
    return diff;
}

int main() {
    srand(time(0));
    for (int i = 0; i < 1000000; ++i) {
        int x = rand() % INT_MAX;
        int y = rand() % INT_MAX;
        // Use XOR to find bit positions that differ
        assert(countSetBits(std::bitset<32>(x ^ y).to_string()) ==
               countDiffBits(x, y));
    }
    return 0;
}
