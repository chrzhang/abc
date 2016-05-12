#include <iostream>
#include <cassert>
#include <string>
#include <cctype>

// Categorize text based on average word length where the following are defined:
// token - chars bounded by spaces or the beginning and end of the input String
// word - token with only letters and may end with a single period and at least
//        1 letter
// word length - number of letters in a word

int wordLenAux(const std::string::const_iterator & begin_it,
               const std::string::const_iterator & end_it) {
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

int wordLen(const std::string & str) {
    if (str.empty()) { return 0; }
    // May start with as many spaces
    auto begin_it = str.begin();
    while (begin_it != str.end() && isspace(*begin_it)) { ++begin_it; }
    if (begin_it == str.end()) { return 0; }
    auto end_it = begin_it;
    while ((end_it != str.end()) && !isspace(*end_it)) { ++end_it; }
    return wordLenAux(begin_it, end_it);
}

int avgWordLen(const std::string & str) {
    if (str.empty()) { return 0; }
    int wc = 0;
    int totalWordLen = 0;
    for (auto it = str.begin(); it != str.end();) {
        if (!isspace(*it)) {
            auto begin_it = it;
            auto end_it = it;
            while (end_it != str.end() && !isspace(*end_it)) { ++end_it; }
            int wordLen = wordLenAux(begin_it, end_it);
            if (wordLen) {
                ++wc;
                totalWordLen += wordLen;
            }
            it = end_it;
        } else {
            ++it;
        }
    }
    if (wc == 0) { return 0; }
    return totalWordLen / wc;
}

int category(const std::string & str) {
    int awl = avgWordLen(str);
    if (awl < 4) { return 250; }
    if (awl < 6) { return 500; }
    return 1000;
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
    assert(23 / 5 == avgWordLen("This is a problem statement"));
    assert(500 == category("This is a problem statement"));
    assert(0 == avgWordLen("523hi"));
    assert(250 == category("523hi"));
    assert(38 / 7 == avgWordLen("Implement a class H5 which contains some "
                                "method."));
    assert(500 == category("Implement a class H5 which contains some "
                           "method."));

    assert(0 == avgWordLen(" no9 . wor7ds he8re. hj.."));
    assert(250 == category(" no9 . wor7ds he8re. hj.."));
    return 0;
}
