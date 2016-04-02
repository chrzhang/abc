#include <iostream>
#include <cassert>
#include <cmath>

// Check if a number is a power of 3

bool isPowerOfThree(int n) {
    if (n <= 0) { return false; }
    int logarithm = (int) (log10(n) / log10(3));
    int r = 1;
    for (int i = 0; i < logarithm; ++i) {
        r *= 3;
    }
    return r == n;
}

int main() {
    int r = 3;
    for (int i = 0; i < 10; ++i) {
        assert(isPowerOfThree(3));
        r *= 3;
    }
    return 0;
}
