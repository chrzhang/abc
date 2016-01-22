#include <iostream>
#include <cstdlib>
#include <cmath>
#include <ctime>
#include <bitset>
#include <string>
#include <assert.h>

#define NUM_BITS 8
#define NUM_ITERATIONS 100

// Swap every pair of bits (first with second, third with fourth...)

std::string getRandomBits() {
    std::string s;
    for (int i = 0; i < NUM_BITS; ++i) {
        s.push_back(rand() % 2 ? '0' : '1');
    }
    return s;
}

std::string getRepeatingMask(const std::string & mask) {
    std::string s;
    while (s.size() < NUM_BITS) {
        s += mask;
    }
    assert(s.size() == NUM_BITS);
    return s;
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        std::bitset<NUM_BITS> x(getRandomBits());
        std::cout << x.to_string() << std::endl;
        // Attempt in as few ops possible (two SHIFTs, two ANDs, one OR)
        x = ((x >> 1) & std::bitset<NUM_BITS>(getRepeatingMask("01"))) |
            ((x << 1) & std::bitset<NUM_BITS>(getRepeatingMask("10")));
        std::cout << x.to_string() << std::endl << std::endl;
    }
    return 0;
}
