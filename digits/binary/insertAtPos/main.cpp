#include <iostream>
#include <cmath>
#include <assert.h>

// Given 2 32-bit numbers and positions, insert one into the other at the
// given positions

int main() {
    int N = 1024; // 10000000000
    int M = 19; // 10011
    // Positions are indexed at 0 on the rightmost digit
    const int i = 2;
    const int j = 6;
    // These positions mean
    // 10000000000
    //     ^^^^^
    //     10011
    assert(i < j);
    // Zero positions i through j in N
    int mask = pow(2, ((j - i) + 1)) - 1;
    mask <<= i;
    N &= ~mask;
    // Left shift M to align the cleared region
    M <<= i;
    std::cout << (N | M) << std::endl;
    assert((N | M) == 1100); // 10001001100
    return 0;
}
