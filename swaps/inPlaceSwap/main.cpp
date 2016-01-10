#include <iostream>
#include <ctime>
#include <cstdlib>
#include <assert.h>

void swap(int & i, int & j) { // Beware of overflow when adding
    i += j;
    j = i - j;
    i -= j;
}

int main() {
    int i, j;
    i = 1; j = 2;
    swap(i, j);
    assert(i == 2);
    assert(j == 1);
    return 0;
}
