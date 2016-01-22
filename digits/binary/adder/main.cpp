#include <iostream>
#include <cstdlib>
#include <ctime>
#include <cassert>

// Add numbers without arithmetic operators

int add(int a, int b) { // Passed in as copies so modify as needed
    while (b > 0) {
        int carry = a & b; // Carries are caused when both bits are set
        a = a ^ b; // Addition without taking into consideration carries
        b = carry << 1; // Shift the existing carries over to be added as 'b'
    }
    return a;
}

int main() {
    for (int i = 0; i < 1000; ++i) {
        for (int j = 0; j < 1000; ++j) {
            assert(i + j == add(i, j));
        }
    }
}
