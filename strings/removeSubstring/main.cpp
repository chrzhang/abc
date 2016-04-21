#include <iostream>
#include <cassert>
#include <string>
#include <algorithm>

// Remove a substring so that the final result does not contain the substring

// Ignore case when looking for substring
std::string::iterator findCaseInsensitive(std::string & str,
                                          const std::string & substr) {
    auto it = std::search(str.begin(), str.end(), substr.begin(), substr.end(),
                          [](char c1, char c2) { return std::tolower(c1) ==
                                                        std::tolower(c2); });
    return it;
}

void removeSubstring(std::string & str, const std::string & substr) {
    std::cout << "Original: |" << str << "|\n";
    auto sought = findCaseInsensitive(str, substr);
    while (sought != str.end()) {
        str.erase(sought, next(sought, substr.length()));
        sought = findCaseInsensitive(str, substr);
    }
    std::cout << "Filtered: |" << str << "|\n";
}

int main() {
    {
        std::string str = "strSTRINGing";
        std::string substr = "string";
        removeSubstring(str, substr);
        assert(str.empty());
    }
    {
        std::string str = "ccccstring ssssssssss";
        std::string substr = "StRinG";
        removeSubstring(str, substr);
        assert(str.compare("cccc ssssssssss") == 0);
    }
    {
        std::string str = "ccccstrinstringgsssssssssss";
        std::string substr = "StRinG";
        removeSubstring(str, substr);
        assert(str.compare("ccccsssssssssss") == 0);
    }
    {
        std::string str = "ccccstring stringssssssssss";
        std::string substr = "StRinG";
        removeSubstring(str, substr);
        assert(str.compare("cccc ssssssssss") == 0);
    }
    return 0;
}
