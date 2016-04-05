#include <iostream>
#include <cassert>
#include <string>

// Count how many clicks it would take to encode a word given one button so
// A - 1 click, B - 2 clicks, and so forth

int numClicks(const std::string & s) {
    int r = 0;
    for (auto c : s) {
        r += c - 'A' + 1;
    }
    return r;
}

int main() {
    assert(1 == numClicks("A"));
    assert(6 == numClicks("ABC"));
    assert(143 == numClicks("VAMOSGIMNASIA"));
    assert(96 == numClicks("TOPCODER"));
    assert(183 == numClicks("SINGLEROUNDMATCH"));
    assert(1300 ==
           numClicks("ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"));
    return 0;
}
