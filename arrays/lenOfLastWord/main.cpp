#include <iostream>
#include <string>
#include <cassert>

// Find the length of the last 'word' (a sequence of non-space chars)

int lengthOfLastWord(std::string s) {
    int r = 0;
    for (auto it1 = s.rbegin(); it1 != s.rend(); ++it1) {
        if (*it1 != ' ') {
            for (auto it2 = it1; it2 != s.rend(); ++it2) {
                if (*it2 == ' ') {
                    break;
                }
                ++r;
            }
            return r;
        }
    }
    return r;
}

int main() {
    assert(lengthOfLastWord("") == 0);
    assert(lengthOfLastWord(" ") == 0);
    assert(lengthOfLastWord("  ") == 0);
    assert(lengthOfLastWord("hi") == 2);
    assert(lengthOfLastWord("hello world") == 5);
    assert(lengthOfLastWord("hello world!") == 6);
    assert(lengthOfLastWord("goodbye, underworld!") == 11);
    return 0;
}
