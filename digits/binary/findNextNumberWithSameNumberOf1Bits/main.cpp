#include <iostream>
#include <string>
#include <bitset>
#include <cmath>
#include <assert.h>

// Find the next biggest, next smallest integer with the same amount of 1 bits

std::string toBin(int n) {
    return std::bitset<16>(n).to_string();
}

int fromBin(const std::string & s) {
    int acc = 0;
    size_t currPow = 0;
    for (auto it = s.rbegin(); it != s.rend(); ++it) {
        if (*it == '1') {
            acc += pow(2, currPow);
        }
        ++currPow;
    }
    return acc;
}

size_t numberOf1s(const std::string & s) {
    size_t a = 0;
    for (auto it = s.begin(); it != s.end(); ++it) {
        a += *it == '1' ? 1 : 0;
    }
    return a;
}

int findNextNumber(int n) {
    // We must change the value so a bit must be flipped 0 to 1 and 1 to 0
    // To make the number bigger, the bit that is flipped to 1 must be to the
    // left of the 1 to 0 bit
    // Flip the rightmost zero that is not trailing (smallest increase)
    auto s = toBin(n);
    bool isTrailing = true;
    auto rit = s.rbegin();
    for (; rit != s.rend(); ++rit) {
        if (*rit != '0') {
            isTrailing = false;
        }
        if (!isTrailing && *rit == '0') {
            *rit = '1';
            break;
        }
    }
    // Count all zeroes and 1s to the right of the flipped bit
    size_t num0s, num1s;
    num0s = num1s = 0;
    auto rit2 = rit;
    while (rit2 != s.rbegin()) {
        --rit2;
        *rit2 == '0' ? ++num0s : ++ num1s;
    }
    // Fill the left with num0s + 1 0s
    for (int i = 0; i < num0s + 1; i++) {
        --rit;
        *rit = '0';
    }
    // Fill the right with num1s - 1 1s (we added an extra 1 before)
    for (int i = 0; i < num1s - 1; i++) {
        --rit;
        *rit = '1';
    }
    return fromBin(s);
}

int findPrevNumber(int n) {
    // A variant of findNextNumber with '0's are subbed with '1's
    auto s = toBin(n);
    bool isTrailing = true;
    auto rit = s.rbegin();
    for (; rit != s.rend(); ++rit) {
        if (*rit != '1') {
            isTrailing = false;
        }
        if (!isTrailing && *rit == '1') {
            *rit = '0';
            break;
        }
    }
    size_t num0s, num1s;
    num0s = num1s = 0;
    auto rit2 = rit;
    while (rit2 != s.rbegin()) {
        --rit2;
        *rit2 == '0' ? ++num0s : ++num1s;
    }
    for (int i = 0; i < num1s + 1; i++) {
        --rit;
        *rit = '1';
    }
    for (int i = 0; i < num0s - 1; i++) {
        --rit;
        *rit = '0';
    }
    return fromBin(s);
}

int main() {
    for (int i = 1; i < 10000; ++i) {
        std::cout << i << std::endl;
        if (i == 0) {
            std::cout << "0 is the only number with 0 1 bits.\n";
            return -1;
        }
        int numSetBits = numberOf1s(toBin(i));
        int j = i + 1;
        while (1) {
            auto s = toBin(j);
            if (numberOf1s(s) == numSetBits) {
                std::cout << "\tAfter: " << j << "(" << s << ")" << std::endl;
                assert(j == findNextNumber(i));
                break;
            }
            ++j;
        }
        int h = i - 1;
        while (h > 0) { // Overflow and negatives are not handled
            auto s = toBin(h);
            if (numberOf1s(s) == numSetBits) {
                std::cout << "\tBefore: " << h << "(" << s << ")" << std::endl;
                assert(h == findPrevNumber(i));
                break;
            }
            --h;
        }
    }
    return 0;
}
