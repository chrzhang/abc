#include <iostream>
#include <string>
#include <climits>

// Determine if a string has all unique characters

bool hasUniqueChars(const std::string & s) {
    bool alphabet[CHAR_MAX - CHAR_MIN] = { false };
    for (std::string::const_iterator it = s.begin(); it != s.end(); ++it) {
        if (alphabet[(unsigned char) *it]) {
            return false;
        }
        alphabet[(unsigned char) *it] = true;
    }
    return true;
}

int main() {
    std::string inputLine;
    while (true) {
        std::cout << "Enter a string (Ctrl + C to quit): ";
        std::getline (std::cin, inputLine);
        if (hasUniqueChars(inputLine)) {
            std::cout << "unique" << std::endl;
        } else {
            std::cout << "not unique" << std::endl;
        }
        std::cout << std::endl;
    }
    return 0;
}
