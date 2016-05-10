#include <iostream>
#include <string>
#include <cassert>

/*
 Vigenere Cipher
 A code word (key) is written repeatedly over a message
 Plaintext: hello
 Key: bye
 Repeated:  byeby
 For each letter in the plaintext, find the corresponding repeated-key letter
 and encrypt by finding intersection in table formed by staggered alphabet
 (rows are letters in code, columns are letters in plaintext)
   A B C D ... Z
 A A B C D ... Z
 B B C D E ... A
 C C D E F ... B
 D D E F G ... C
 . . . . .     .
 . . . . .     .
 . . . . .     .
 Z Z A B C ... Y
*/

std::string encode(const std::string & str, const std::string & key) {
    if (str.empty() || key.empty()) { return ""; }
    for (auto c : str) {
        if (!isalpha(c)) { return ""; }
        if (!isupper(c)) { return ""; }
    }
    for (auto c : key) {
        if (!isalpha(c)) { return ""; }
        if (!isupper(c)) { return ""; }
    }
    // String validated
    std::string result;
    result.reserve(str.size());
    for (auto it = str.begin(); it != str.end(); ++it) {
        char keyChar = key[(it - str.begin()) % key.size()];
        result.push_back((char)((((keyChar - 'A') + *it) - 'A') % 26 + 'A'));
    }
    return result;
}

std::string decode(const std::string & str, const std::string & key) {
    if (str.empty() || key.empty()) { return ""; }
    for (auto c : str) {
        if (!isalpha(c)) { return ""; }
        if (!isupper(c)) { return ""; }
    }
    for (auto c : key) {
        if (!isalpha(c)) { return ""; }
        if (!isupper(c)) { return ""; }
    }
    // String validated
    std::string result;
    result.reserve(str.size());
    for (auto it = str.begin(); it != str.end(); ++it) {
        char keyChar = key[(it - str.begin()) % key.size()];
        int diff = *it - keyChar;
        if (keyChar > *it) { // Need wraparound
            diff = 26 - (keyChar - *it);
        }
        result.push_back(diff + 'A');
    }
    return result;
}

int main() {
    assert("VCSGQRHVKGJVGOW" == encode("TOPCODERISGREAT", "CODE"));
    assert("TOPCODERISGREAT" == decode("VCSGQRHVKGJVGOW", "CODE"));
    return 0;
}
