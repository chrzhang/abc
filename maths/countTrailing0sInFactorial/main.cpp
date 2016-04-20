#include <iostream>
#include <assert.h>
#include <gmpxx.h>

#define N 1000 // Use GMP library with C++ interface for arbitrary precision

// Count trailing zeroes in N!

mpz_class fact(mpz_class n) {
    if (n == 0) { return 1; }
    return n * fact(n - 1);
}

mpz_class countTrailingZeroesIn(mpz_class n) {
    if (n == 0) { return 1; }
    mpz_class counter = 0;
    while (n % 10 == 0) {
        ++counter;
        n /= 10;
    }
    return counter;
}

mpz_class min(mpz_class i, mpz_class j) {
    return i > j ? j : i;
}

mpz_class countOccIn(mpz_class factor, mpz_class n) {
    mpz_class ct = 0;
    while (n % factor == 0) {
        n /= factor;
        ++ct;
    }
    return ct;
}

mpz_class countTrailingZeroesInFactorialOf(mpz_class n) {
    if (n == 0) { return 0; }
    // A trailing zero is introduced when the multiplication involves a 10
    // formed by multiplying 5 by 2 (thus we count the # of 5s and 2s)
    mpz_class numberOf2s = 0;
    mpz_class numberOf5s = 0;
    for (int i = 1; i <= n; ++i) {
        numberOf2s += countOccIn(2, i);
        numberOf5s += countOccIn(5, i);
    }
    return min(numberOf5s, numberOf2s);
}

int main() {
    for (mpz_class i = 0; i <= N; ++i) {
        assert(countTrailingZeroesIn(fact(i)) ==
               countTrailingZeroesInFactorialOf(i));
    }
    return 0;
}
