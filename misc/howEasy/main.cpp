#include <iostream>
#include <cassert>
#include <string>
#include <cctype>

// Categorize text based on average word length where the following are defined:
// token - chars bounded by spaces or the beginning and end of the input String
// word - token with only letters and may end with a single period and at least
//        1 letter
// word length - number of letters in a word

int wordLen(const std::string & str) {
    // May start with as many spaces
    auto begin_it = str.begin();
    while (begin_it != str.end() && isspace(*begin_it)) { ++begin_it; }
    if (begin_it == str.end()) { return 0; }
    auto end_it = begin_it;
    while ((end_it != str.end()) && !isspace(*end_it)) { ++end_it; }
    bool foundLetter = false;
    for (auto it = begin_it; it != end_it; ++it) {
        if (*it == '.') { // May end with a single period
            if ((std::next(it) == end_it) && foundLetter) {
                return std::distance(begin_it, end_it) - 1;
            } else {
                return 0;
            }
        }
        if (!isalpha(*it)) {
            return 0;
        } else {
            foundLetter = true;
        }
    }
    if (foundLetter) {
        return std::distance(begin_it, end_it);
    } else {
        return 0;
    }
}

int main() {
    assert(wordLen("ab") == 2);
    assert(wordLen("ab.") == 2);
    assert(!wordLen("ab..")); // Ends in more than one '.'
    assert(!wordLen("a.b")); // Can only end with a '.'
    assert(!wordLen(".ab"));
    assert(!wordLen("a.b.")); // More than one '.'
    assert(!wordLen("a2b.")); // Only letters are okay
    assert(!wordLen(".")); // Needs one letter at the least
    return 0;
}
