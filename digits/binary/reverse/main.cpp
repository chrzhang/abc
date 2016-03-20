#include <iostream>
#include <algorithm>
#include <string>
#include <bitset>
#include <cassert>

// Reverse bits of a 32-bit int

uint32_t reverseBits(uint32_t n) {
    uint32_t result = 0;
    for (int i = 0; i < 32; ++i) {
        result <<= 1;
        result |= n & 1;
        n >>= 1;
    }
    return result;
}

int main() {
    // Note 0xFFFFFFFF is 4,294,967,295
    for (uint32_t i = 0; i < 0xFFFFFFFF; ++i) { // Complete test coverage
        std::cout << i << "\r";
        std::string bin = std::bitset<32>(i).to_string();
        std::reverse(bin.begin(), bin.end());
        assert(std::bitset<32>(reverseBits(i)).to_string() == bin);
    }
    std::cout << std::endl;
    return 0;
}
