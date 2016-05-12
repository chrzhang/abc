#include <iostream>
#include <cassert>
#include <string>
#include <cctype>

// Categorize text based on average word length where the following are defined:
// token - chars bounded by spaces or the beginning and end of the input String
// word - token with only letters and may end with a single period and at least
//        1 letter
// word length - number of letters in a word

bool isWord(const std::string & str) {
    // May start with as many spaces
    auto begin_it = str.begin();
    while (begin_it != str.end() && isspace(*begin_it)) { ++begin_it; }
    if (begin_it == str.end()) { return false; }
    auto end_it = begin_it;
    while ((end_it != str.end()) && !isspace(*end_it)) { ++end_it; }
    bool foundLetter = false;
    for (auto it = begin_it; it != end_it; ++it) {
        if (*it == '.') { // May end with a single period
            return (std::next(it) == end_it) && foundLetter;
        }
        if (!isalpha(*it)) {
            return false;
        } else {
            foundLetter = true;
        }
    }
    return foundLetter;
}

int main() {
    assert(isWord("ab"));
    assert(isWord("ab."));
    assert(!isWord("ab..")); // Ends in more than one '.'
    assert(!isWord("a.b")); // Can only end with a '.'
    assert(!isWord(".ab"));
    assert(!isWord("a.b.")); // More than one '.'
    assert(!isWord("a2b.")); // Only letters are okay
    assert(!isWord(".")); // Needs one letter at the least
    return 0;
}
