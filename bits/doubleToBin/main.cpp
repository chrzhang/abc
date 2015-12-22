#include <iostream>
#include <string>
#include <cmath>
#include <assert.h>
#include <cstdlib>
#include <ctime>
#include <limits.h>

#define NUM_DOUBLES 1000000

// Convert a double between 0 and 1 to its binary representation

std::string doubleToBin(double n) {
    std::string s;
    int currPow = -1;
    while (n > 0) {
        double curr2RaisedToPow = pow(2, currPow);
        if (n >= curr2RaisedToPow) {
            s.push_back('1');
            n -= curr2RaisedToPow;
        } else {
            s.push_back('0');
        }
        --currPow;
    }
    return s;
}

double binToDouble(const std::string & s) {
    int currPow = -1;
    double acc = 0;
    for (auto it = s.begin(); it != s.end(); ++it) {
        if (*it == '1') {
            acc += pow(2, currPow);
        }
        --currPow;
    }
    return acc;
}

int main() {
    for (int i = 0; i < NUM_DOUBLES; ++i) {
        double d = (double) rand() / (double) RAND_MAX;
        assert(d == binToDouble(doubleToBin(d)));
    }
    return 0;
}
