#include <iostream>
#include <assert.h>

#define N 12 // Any more won't be valid because of overflow

// Count trailing zeroes in N!

unsigned fact(unsigned n) {
    if (n == 0) { return 1; }
    return n * fact(n - 1);
}

unsigned countTrailingZeroesIn(unsigned n) {
    if (n == 0) { return 1; }
    unsigned counter = 0;
    while (!(n % 10)) {
        ++counter;
        n /= 10;
    }
    return counter;
}

unsigned countTrailingZeroesInFactorialOf(unsigned n) {
    if (N == 0) { return 0; }
    unsigned currLastDigit = 1;
    unsigned numTrailingZeroes = 0;
    for (unsigned i = 1; i <= n; ++i) {
        auto j = i;
        auto tz = countTrailingZeroesIn(j);
        numTrailingZeroes += tz;
        if (tz) {
            j /= 10 * tz;
        }
        if (currLastDigit) { // Multiplying by 0 will not introduce new 0s
            currLastDigit *= j % 10;
            currLastDigit %= 10;
            numTrailingZeroes += currLastDigit == 0 ? 1 : 0;
        }
    }
    return numTrailingZeroes;
}

int main() {
    for (unsigned i = 0; i <= N; ++i) {
        assert(countTrailingZeroesIn(fact(i)) ==
               countTrailingZeroesInFactorialOf(i));
    }
    return 0;
}
