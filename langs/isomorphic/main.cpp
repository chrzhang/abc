#include <iostream>
#include <string>
#include <unordered_map>
#include <cassert>

// Find if strings are isomorphic (replacing a given character with another or
// itself consistently can make the strings equal eg. aa and bb or abc and xyz)

bool isIsomorphic(std::string s, std::string t) {
    if (s.empty() && t.empty()) { return true; }
    assert(s.size() == t.size());
    int sLastMapping, tLastMapping;
    sLastMapping = tLastMapping = 0;
    // Keep a mapping of each char to an int ID
    std::unordered_map<char, int> sIds;
    std::unordered_map<char, int> tIds;
    auto sit = s.begin();
    auto tit = t.begin();
    while (sit != s.end()) { // Traverse through strings one char at a time
        auto seekS = sIds.find(*sit);
        auto seekT = tIds.find(*tit);
        if (seekS == sIds.end()) {
            sIds[*sit] = ++sLastMapping;
        } else {
            sLastMapping = seekS->second;
        }
        if (seekT == tIds.end()) {
            tIds[*tit] = ++tLastMapping;
        } else {
            tLastMapping = seekT->second;
        }
        if (tLastMapping != sLastMapping) { return false; }
        ++sit;
        ++tit;
    }
    return true;
}

int main() {
    assert(isIsomorphic("egg", "add"));
    assert(!isIsomorphic("foo", "bar"));
    assert(isIsomorphic("paper", "title"));
    return 0;
}
