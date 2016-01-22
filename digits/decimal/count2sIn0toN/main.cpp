#include <iostream>
#include <cassert>

// Find the number of 2s in the digit representations of [0..N]

int min(int a, int b) {
    return a < b ? a : b;
}

int max(int a, int b) {
    return a > b ? a : b;
}

int count2sIn(int j) {
    int num2s = 0;
    while (j) {
        if (j % 10 == 2 ) { ++num2s; }
        j /= 10;
    }
    return num2s;
}

int bfCount2sIn0To(const int n) { // Used to test other implementations
    int num2s = 0;
    for (int i = 0; i <= n; ++i) {
        num2s += count2sIn(i);
    }
    return num2s;
}

int count2sIn0To(const int n) {
    if (n < 10) { return n >= 2; }
    int num2s = 0;
    int magnitude = 10;
    // Examine the numbers 0 to N first by their position from ones to tens...
    // The ones position corresponds to a magnitude of 10, the tens 100s...
    while (n * 10 >= magnitude) {
        // Count how many repeating sets of 0-9 digits there are
        int numFull = n / magnitude;
        // For the ones digit position, a 2 repeats every 10 digits so an n
        // of 50 which has 5 full sets of 10 will have 10 2s
        num2s += (magnitude / 10) * numFull;
        // Now see how many were left out of consideration from a full set
        int leftover = 1 + (n % magnitude); // Because digits flow 0 to 9
        // The leftover portion is part of a sequence of
        // (mag / 10) 0s, 1s, 2s, 3s, 4s, 5s, 6s, 7s, 8s, 9s
        num2s += max(0, min(magnitude / 10, leftover - 2 * (magnitude / 10)));
        magnitude *= 10;
    }
    return num2s;
}

int main() {
    for (int i = 0; i < 1000000; ++i) {
        std::cout << "Testing N = " << i << "\r";
        // Slow test
        assert(count2sIn0To(i) == bfCount2sIn0To(i));
    }
    return 0;
}
