#include <iostream>
#include <cassert>
#include <string>

// Given classes that have pre-requisites, find a valid ordering to take the
// classes (if there is no strict ordering between two or more classes, use the
// class number to order and then if the numbers match, the alphabetical order
// of the department)

struct Class {
    std::string dept;
    int num;
    Class(const std::string & s, int n) : dept(s), num(n) {}
};

// 3 or 4 characters of capital letters
bool isValidDept(const std::string & s) {
    if (s.size() < 3 || s.size() > 4) { return false; }
    for (auto c : s) {
        if (!isalpha(c) || !isupper(c)) { return false; }
    }
    return true;
}

bool isValidNum(int n) {
    return n >= 100 && n <= 999;
}

bool isValidListing(const std::string & s) {
    auto it = s.begin();
    while (isalpha(*it) && isupper(*it)) { ++it; }
    if (!isValidDept(std::string(s.begin(), it))) { return false; }
    return isValidNum(std::stoi(std::string(it, s.end())));
}

int main() {
    assert(isValidListing("CSE111"));
    assert(isValidListing("CSE110"));
    assert(isValidListing("MATH101"));
    assert(!isValidListing("CS117"));
    assert(!isValidListing("cs117"));
    assert(!isValidListing("CS9E11"));
    assert(!isValidListing("MAM2222"));
    assert(!isValidListing("MAM22"));
    assert(!isValidListing("ENGIN517"));
    return 0;
}
