#include <iostream>
#include <string>
#include <cassert>
#include <cstdlib>
#include <ctime>
#include <stdexcept>

#define NUM_ITERATIONS 1000000

// Add strings of 1s and 0s as if they were bits

std::string addBinary(const std::string & a, const std::string & b) {
    std::string r;
    auto aIt = a.rbegin();
    auto bIt = b.rbegin();
    int carry = 0;
    while (!(aIt == a.rend() && bIt == b.rend())) {
        char c1, c2;
        c1 = c2 = '0';
        if (aIt != a.rend()) {
            c1 = *aIt;
            ++aIt;
        } else {
            assert(c1 == '0');
        }
        if (bIt != b.rend()) {
            c2 = *bIt;
            ++bIt;
        } else {
            assert(c2 == '0');
        }
        int i1 = c1 == '0' ? 0 : 1;
        int i2 = c2 == '0' ? 0 : 1;
        switch (i1 + i2 + carry) {
            case 0: {
                r.push_back('0');
                carry = 0;
                break;
            }
            case 1: {
                r.push_back('1');
                carry = 0;
                break;
            }
            case 2: {
                r.push_back('0');
                carry = 1;
                break;
            }
            case 3: {
                r.push_back('1');
                carry = 1;
                break;
            }
        };
    }
    if (carry) {
        r.push_back('1');
    }
    return std::string(r.rbegin(), r.rend());
}

std::string binary(unsigned x) {
    std::string r;
    do {
        r.push_back('0' + (x & 1));
    } while (x >>= 1);
    return std::string(r.rbegin(), r.rend());
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        int a = rand();
        int b = rand();
        auto as = binary(a);
        auto bs = binary(b);
        auto r = addBinary(as, bs);
        try {
            assert(std::stoi(r, nullptr, 2) == a + b);
        } catch (const std::out_of_range & oor) {
            continue;
        }
        std::cout << a << " + " << b << " = " << as << " + " << bs << "\n";
    }
    return 0;
}
