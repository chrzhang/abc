#include <iostream>
#include <cassert>
#include <string>
#include <set>
#include <algorithm>

// Given classes that have pre-requisites, find a valid ordering to take the
// classes (if there is no strict ordering between two or more classes, use the
// class number to order and then if the numbers match, the lexicographic order
// of the department)

struct Class {
    std::string dept;
    int num;
    Class(const std::string & s) {
        auto it = s.begin();
        while (isalpha(*it) && isupper(*it)) { ++it; }
        dept = std::string(s.begin(), it);
        num = std::stoi(std::string(it, s.end()));
    }
};

bool operator<(const Class & c1, const Class & c2) {
    if (c1.num != c2.num) {
        return c1.num < c2.num;
    } else {
        return std::lexicographical_compare(c1.dept.begin(), c1.dept.end(),
                                            c2.dept.begin(), c2.dept.end());
    }
}

// Departments are 3 or 4 characters of capital letters
bool isValidDept(const std::string & s) {
    if (s.size() < 3 || s.size() > 4) { return false; }
    for (auto c : s) {
        if (!isalpha(c) || !isupper(c)) { return false; }
    }
    return true;
}

// Class numbers are in [100, 999]
bool isValidNum(int n) {
    return n >= 100 && n <= 999;
}

bool isValidListing(const std::string & s) {
    auto it = s.begin();
    while (isalpha(*it) && isupper(*it)) { ++it; }
    if (!isValidDept(std::string(s.begin(), it))) { return false; }
    return isValidNum(std::stoi(std::string(it, s.end())));
}

bool isValidInputLine(const std::string & s) {
    if (s.empty()) { return false; }
    bool colonFound = false;
    auto it = s.begin();
    while (it != s.end()) {
        if (*it == ':') {
            if (!isValidListing(std::string(s.begin(), it))) { return false; }
            colonFound = true;
            ++it;
            break;
        }
        ++it;
    }
    if (!colonFound) { return false; }
    if (it == s.end()) { return true; }
    while (it != s.end()) {
        if (*it != ' ') { return false; }
        ++it;
        auto endit = it;
        while (endit != s.end() && *endit != ' ') { ++endit; }
        if (!isValidListing(std::string(it, endit))) { return false; }
        it = endit;
    }
    return true;
}

void readValidInputLine(const std::string & s, std::set<Class> & classes) {
    auto it = s.begin();
    while (it != s.end()) {
        if (*it == ':') {
            classes.insert(Class(std::string(s.begin(), it)));
            ++it;
            break;
        }
        ++it;
    }
    while (it != s.end()) {
        ++it;
        auto endit = it;
        while (endit != s.end() && *endit != ' ') { ++endit; }
        classes.insert(Class(std::string(it, endit)));
        it = endit;
    }
}

int main() {
    assert(isValidInputLine("CSE111: CSE110 MATH101"));
    assert(isValidInputLine("CSE110:"));
    assert(!isValidInputLine("CS117:"));
    assert(!isValidInputLine("cs117:"));
    assert(!isValidInputLine("CS9E11:"));
    assert(!isValidInputLine("CSE110: "));
    assert(!isValidInputLine("CSE110: CSE101 "));
    assert(!isValidInputLine("MATH211: MAM2222"));
    assert(!isValidInputLine("MATH211: MAM22"));
    assert(!isValidInputLine("ENGIN517: MATH211"));
    {
        std::set<Class> classes;
        auto classSchedule = { "CSE121: CSE110",
                               "CSE110:",
                               "MATH122:" };
        for (auto cl : classSchedule) {
            readValidInputLine(cl, classes);
        }
        for (auto c : classes) {
            std::cout << c.dept << c.num << std::endl;
        }
    }
    return 0;
}
