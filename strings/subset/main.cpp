#include <iostream>
#include <string>
#include <map>
#include <cassert>

// Given two strings, find if one string can be formed by selecting and removing
// strings from the second

bool canConstruct(std::string s_dest, std::string s_src) {
    std::map<char, int> occ;
    for (auto c : s_src) {
        occ[c]++;
    }
    for (auto c : s_dest) {
        if (occ[c] == 0) {
            return false;
        }
        --occ[c];
    }
    return true;
}

int main() {
    assert(canConstruct("a", "b") == false);
    assert(canConstruct("aa", "ab") == false);
    assert(canConstruct("aa", "aab") == true);
    return 0;
}
