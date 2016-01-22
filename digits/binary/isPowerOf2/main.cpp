#include <iostream>

// Explain what (n & (n - 1)) == 0 means

int main() {
    int i = 0;
    while (i < 1000000) {
        // A power of 2 is a single 1 followed by n 0s as binary
        // Subtracting 1 from the # gives n 1s as binary
        // Therefore an AND that yields 0 must be from a power of 2
        if (0 == (i & (i - 1))) {
            std::cout << i << "\t";
        }
        ++i;
    }
    std::cout << std::endl;
    return 0;
}
