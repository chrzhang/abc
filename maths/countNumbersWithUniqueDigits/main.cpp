#include <iostream>
#include <cmath>

// Given n, count all numbers with unique digits in [0, 10^n)

int fact(int n) {
    if (n < 0) {
        return -1;
    }
    if (n == 0) {
        return 1;
    } else {
        return n * fact(n - 1);
    }
}

int permutations(int n, int r) {
    return fact(n) / fact(n - r);
}

int countNumbersWithUniqueDigits(int n) {
    if (n < 0) {
        return 0;
    } else if (n == 0) {
        return 1;
    } else if (n == 1) {
        return 10;
    }
    return permutations(9, n) + (n -1) * permutations(9, n - 1) +
           countNumbersWithUniqueDigits(n - 1);
}

int main() {
    for (int n = 0; n < 10; ++n) {
        std::cout << "There are " << countNumbersWithUniqueDigits(n)
                  << " numbers with unique digits in [0, "
                  << pow(10, n) << ")\n";
    }
    return 0;
}
