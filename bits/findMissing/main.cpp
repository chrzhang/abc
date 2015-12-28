#include <iostream>
#include <assert.h>
#include <cmath>
#include <cstdlib>

#define N  10
#define NUM_BITS 8

// Given an array of numbers, find the missing number (can only access jth bit
// of each number in O(1))

class Number {
    int val; // Can only be accessed by asking bit by bit
    public:
        Number() {
            val = -1;
        }
        Number(int val) {
            this->val = val;
        }
        char getJthBit(size_t j) { // Indexed from 0
            return ((1 << j) & val) ? '1' : '0';
        }
        int getVal() const {
            return val;
        }
};

int main() {
    srand(time(0));
    // Create array of size N with a missing value between 0 to N
    int missingIndex = rand() % N;
    Number arr[N]; // To hold 0,1,...,N
    int val = 0;
    for (int i = 0; i < N; ++i) {
        if (i == missingIndex) {
            ++val;
        }
        arr[i] = Number(val++);
        std::cout << "arr[" << i << "] = " << arr[i].getVal() << std::endl;
    }
    // Find the missing value
    size_t amountOf1s[NUM_BITS] = { 0 };
    // Store a counter of how many 1 bits are supposed to be in each position
    // for the numbers from 0 to N without missing values (then count how many
    // are in the array and any discrepancy will help form the missing value)
    for (size_t bitPos = 1, currPowOf2 = 2; bitPos <= NUM_BITS; ++bitPos) {
        size_t leftover = (N + 1) % currPowOf2;
        //std::cout << "\tleftover: " << leftover << std::endl;
        size_t numSets = (N + 1) / currPowOf2;
        //std::cout << "\tnumSets: " << numSets << std::endl;
        size_t num1s = numSets * (currPowOf2 / 2);
        if (leftover / (currPowOf2 / 2)) {
            num1s += leftover % (currPowOf2 / 2);
        }
        amountOf1s[bitPos - 1] = num1s;
        currPowOf2 *= 2;
    }
    // For each bit position, look through every Number in the array
    for (size_t bitPos = 0; bitPos < NUM_BITS; ++bitPos) {
        for (int i = 0; i < N; ++i) {
            amountOf1s[bitPos] -= arr[i].getJthBit(bitPos) == '1' ? 1 : 0;
        }
    }
    int missingNo = 0;
    for (int i = 0; i < NUM_BITS; ++i) {
        missingNo += amountOf1s[i] ? pow(2, i) : 0;
    }
    std::cout << "Missing: " << missingNo << std::endl;
    return 0;
}
