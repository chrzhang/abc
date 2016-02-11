#include <iostream>
#include <string>
#include <cassert>

// Do basic regular expression matching for . and *

bool matchAux(std::string::const_iterator sit,
              std::string::const_iterator regexIt,
              const std::string & str, const std::string & regex) {
    if (sit == str.end()) {
        return regexIt == regex.end();
    }
    auto succ = std::next(regexIt);
    if (*succ != '*') { // Is not followed by *, must match char literal or .
        if ((*sit == *regexIt) || (*regexIt == '.' && sit != str.end())) {
            return matchAux(std::next(sit), std::next(regexIt), str, regex);
        } else {
            return false;
        }
    } else { // Is followed by *, have 0 or more copies of prev match
        while ((*sit == *regexIt) || (*regexIt == '.' && sit != str.end())) {
            if (matchAux(sit, std::next(regexIt, 2), str, regex)) {
                return true;
            }
            sit = std::next(sit);
        }
        return matchAux(sit, std::next(regexIt, 2), str, regex);
    }
}

bool match(const std::string & str, const std::string & regex) {
    return matchAux(str.begin(), regex.begin(), str, regex);
}

int main() {
    assert(!match("aa", "a"));
    assert(!match("aaa", "aa"));
    assert(match("aa", "aa"));
    assert(match("aa", "a*"));
    assert(match("aa", ".*"));
    assert(match("ab", ".*"));
    assert(match("aab", "c*a*b*"));
    return 0;
}
