#include <iostream>
#include <ctime>
#include <cstdlib>
#include <assert.h>

#define NUM_ITERATIONS 1000000

// Swap two numbers in-place (do not use any temporary variables)

void swap(int & i, int & j) { // Beware of overflow when adding
    i += j;
    j = i - j;
    i -= j;
}

int main() {
    srand(time(0));
    int i, j;
    i = 1; j = 2;
    swap(i, j);
    assert(i == 2);
    assert(j == 1);
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        i = rand() % 1000;
        j = rand() % 1000;
        auto oldI = i;
        auto oldJ = j;
        swap(i, j);
        assert(j == oldI);
        assert(i == oldJ);
    }
    return 0;
}
