#include <iostream>
#include <bitset>
#include <string>
#include <cassert>
#include <climits>
#include <chrono>
#include <random>

// Count the number of 1 bits in a 64-bit unsigned integer

unsigned parityTest(uint64_t number) {
    std::bitset<64> bs(number);
    std::cout << bs << std::endl;
    std::string s = bs.to_string();
    unsigned count = 0;
    for (auto c : s) {
        if (c == '1') {
            ++count;
        }
    }
    return count;
}

unsigned parity(uint64_t number) {
    unsigned count = 0;
    while (number) {
        if (number & 1) {
            ++count;
        }
        number >>= 1;
    }
    return count;
}

int main() {
    unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
    std::mt19937_64 gen(seed);
    while (true) {
        unsigned long long i = gen();
        assert(parity(i) == parityTest(i));
    }
    return 0;
}
