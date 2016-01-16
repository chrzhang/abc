#include <iostream>
#include <ctime>
#include <cstdlib>
#include <climits>
#include <cmath>

// Generate a random # from [0..6] given a fair 5-sided die

int rand5() {
    return rand() % 5;
}

int rand7() {
    // Use rand() % 5 to decide bits 0 to 3 by ignoring any non-binary rand #
    int randBit0, randBit1, randBit2;
    randBit0 = randBit1 = randBit2 = 2;
    while (randBit0 > 1) {
        randBit0 = rand5();
    }
    while (randBit1 > 1) {
        randBit1 = rand5();
    }
    while (randBit2 > 1) {
        randBit2 = rand5();
    }
    if (randBit0 == randBit1 && randBit1 == randBit2 && randBit2 == 1) {
        return rand7(); // Re-try since 7 is illegal (only 0 to 6)
    }
    return randBit0 * pow(2, 0) + randBit1 * pow(2, 1) + randBit2 * pow(2, 2);
}

int main() {
    srand(time(0));
    double counts[7];
    for (int i = 0; i < 1000000; ++i) {
        counts[rand7()]++;
    }
    double total = 0;
    for (int i = 0; i < 7; ++i) {
        total += counts[i];
    }
    std::cout << "1/7 = " << 100 * (1.0 / 7.0) << "%" << std::endl;
    for (int i = 0; i < 7; ++i) {
        std::cout << "Chance of rolling a " << i << " is "
                  << (counts[i] / total) * 100 << "%" << std::endl;
    }
    return 0;
}
