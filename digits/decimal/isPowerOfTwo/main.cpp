#include <iostream>
#include <cassert>

// Check if a number is a power of two

bool isPowerOfTwo(int n) {
    if (n == 0) { return false; }
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
    assert(isPowerOfTwo(1));
    assert(isPowerOfTwo(2));
    assert(isPowerOfTwo(4));
    assert(isPowerOfTwo(8));
    assert(isPowerOfTwo(16));
    assert(isPowerOfTwo(32));
    assert(isPowerOfTwo(64));
    assert(isPowerOfTwo(128));
    assert(!isPowerOfTwo(0));
    assert(!isPowerOfTwo(3));
    assert(!isPowerOfTwo(5));
    assert(!isPowerOfTwo(6));
    assert(!isPowerOfTwo(7));
    assert(!isPowerOfTwo(100));
    assert(!isPowerOfTwo(20));
    assert(!isPowerOfTwo(33));
    return 0;
}
