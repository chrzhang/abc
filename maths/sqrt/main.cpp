#include <iostream>
#include <cassert>

// Find the square root of x

int findSqrRoot(int x) {
    if (x < 0) { return -1; }
    if (x == 0) { return 0; }
    for (int i = 1; i <= x; ++i) {
        if (i * i == x) { return i; }
    }
    return -1;
}

int main() {
    assert(findSqrRoot(0) == 0);
    assert(findSqrRoot(1) == 1);
    assert(findSqrRoot(4) == 2);
    assert(findSqrRoot(9) == 3);
    assert(findSqrRoot(16) == 4);
    assert(findSqrRoot(25) == 5);
    return 0;
}
