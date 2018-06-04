#include <iostream>
#include <cassert>
#include <fstream>
#include <sstream>

using namespace std;

int toInt(const char c) {
    assert(c >= '0' && c <= '9');
    return c - '0';
}

int countCarriesFromAdding(const string & a, const string & b) {
    int carryCount = 0;
    if (a.empty() || b.empty()) {
        return carryCount;
    }
    int carry = 0;
    for (auto a_it = a.rbegin(), b_it = b.rbegin();
         a_it != a.rend() || b_it != b.rend();) {
        char aValue = '0';
        if (a_it != a.rend()) {
            aValue = *a_it;
        }
        char bValue = '0';
        if (b_it != b.rend()) {
            bValue = *b_it;
        }
        carry = (carry + toInt(aValue) + toInt(bValue)) / 10;
        carryCount += carry;
        if (a_it != a.rend()) {
            ++a_it;
        }
        if (b_it != b.rend()) {
            ++b_it;
        }
    }
    return carryCount;
}

void testBulk() {
    ifstream inFile("input.txt");
    string currLine;
    while (getline(inFile, currLine)) {
        string aStr, bStr;
        stringstream ss(currLine);
        ss >> aStr;
        ss >> bStr;
        if (aStr == "0" && bStr == "0") {
            return;
        }
        const int carryCount = countCarriesFromAdding(aStr, bStr);
        if (0 == carryCount) {
            cout << "No carry operation." << endl;
        } else {
            cout << carryCount << " carry operation" << (carryCount > 1 ? "s" : "") << "." << endl;
        }
    }
}

int main() {
    assert(0 == countCarriesFromAdding("123", "456"));
    assert(3 == countCarriesFromAdding("555", "555"));
    assert(1 == countCarriesFromAdding("123", "594"));
    testBulk();
    return 0;
}
