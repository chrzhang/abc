#include <iostream>
#include <cstdlib>
#include <ctime>
#include <assert.h>

#define NUM_ITERATIONS 1000000

// Find the larger of 2 numbers without using conditionals or comparisons

int getMax(int a, int b) {
    auto diff = a - b;
    diff >>= (sizeof(int) * 8) - 1;
    diff &= 1;
    return (diff * b + (diff ^ 1) * a);
}

int testMax(int a, int b) {
    return a > b ? a : b;
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        int a = rand() % 1000000;
        int b = rand() % 1000000;
        assert(getMax(a,b) == testMax(a,b));
    }
    return 0;
}
