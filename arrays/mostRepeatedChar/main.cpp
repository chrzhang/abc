#include <iostream>
#include <string>
#include <cassert>

// Find the character that appears the most in an alphanumeric string

int getIndexOf(char c) {
    if (islower(c)) {
        return c - 'a';
    } else if (isupper(c)) {
        return (c - 'A') + 26;
    } else if (isdigit(c)) {
        return (c - '0') + 26 + 26;
    } else {
        return -1;
    }
}

char mostPopularCharIn(const std::string & str) {
    // alphabet is filled with 'a-zA-Z0-9'
    int counts[62] = { 0 }; // 26 + 26 + 10
    char currMaxChar = 0;
    for (auto c : str) {
        auto i = getIndexOf(c);
        counts[i]++;
        if (currMaxChar == 0) {
            currMaxChar = c;
            continue;
        }
        if (counts[i] > counts[getIndexOf(currMaxChar)]) {
            currMaxChar = c;
        }
    }
    std::cout << "Most frequent char in |" << str << "| is " << currMaxChar
              << "\n";
    return currMaxChar;
}

int main() {
    assert('c' == mostPopularCharIn("aaiicccnn"));
    assert('a' == mostPopularCharIn("aabbccdd"));
    assert('2' == mostPopularCharIn("ab2sbf2dj2skl"));
    return 0;
}
