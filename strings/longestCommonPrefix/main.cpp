#include <iostream>
#include <string>
#include <vector>
#include <cassert>

// Find the longest common substring between strings

std::string findLongestCommonPrefix(const std::vector<std::string> & v) {
    std::string prefix;
    if (v.empty()) { return prefix; }
    for (size_t currIndex = 0; ; ++currIndex) {
        if (currIndex >= v[0].size()) { return prefix; }
        char c = v[0][currIndex];
        for (size_t i = 1; i < v.size(); ++i) {
            if (currIndex >= v[i].size()) { return prefix; }
            if (v[i][currIndex] != c) { return prefix; }
        }
        prefix.append(1, c);
    }
}

int main() {
    std::vector<std::string> v = { "oranguatan", "orange" };
    assert(findLongestCommonPrefix(v) == "orang");
    v.push_back("orifice");
    assert(findLongestCommonPrefix(v) == "or");
    v.push_back("onerous");
    assert(findLongestCommonPrefix(v) == "o");
    v.push_back("a");
    assert(findLongestCommonPrefix(v) == "");
    return 0;
}
