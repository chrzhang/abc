#include <iostream>
#include <cassert>

// Explain what (n & (n - 1)) == 0 means

bool isPowerOf2Version1(int n) {
    return 0 == (n & (n - 1));
}

bool isPowerOf2Version2(int n) {
    // Has only one 1 in its binary representation
    bool foundOnBit = 0;
    while (1) {
        if (n & 1) {
            if (foundOnBit) { return false; }
            foundOnBit = true;
        }
        n >>= 1;
        if (0 == n) {
            return true;
        }
    }
}


int main() {
    int i = 0;
    while (i < 1000000) {
        // A power of 2 is a single 1 followed by n 0s as binary
        // Subtracting 1 from the # gives n 1s as binary
        // Therefore an AND that yields 0 must be from a power of 2
        if (isPowerOf2Version1(i)) {
            assert(isPowerOf2Version2(i));
            std::cout << i << "\n";
        }
        ++i;
    }
    std::cout << std::endl;
    return 0;
}
