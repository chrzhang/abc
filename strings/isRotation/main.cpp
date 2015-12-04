#include <iostream>
#include <stdio.h>
#include <cstring>
#include <string>

// Given isSubstring, write isRotation to see if a string is a rotation.

bool isSubstring(const char * s1, const char * s2) { // True if s1 has s2
    return nullptr != strstr(s1, s2);
}

bool isRotation(const char * s1, const char * s2) {
    // Iterate through s1 for the start of s2
    std::string buffer; // Use std::string for brevity as a buffer
    const char * p1 = s1;
    while (*p1) {
        if (*p1 == *s2) { // Found the start
            // If what follows and what comes before are substrings, is rotation
            if (isSubstring(s2, p1) && isSubstring(s2, buffer.c_str())) {
                return true;
            }
        } else {
            buffer.append(1, *p1);
        }
        ++p1;
    }
    return false;
}

int main() {
    while (true) {
        std::string input1, input2;
        std::cout << "Enter a string: ";
        getline(std::cin, input1);
        std::cout << "Enter another string: ";
        getline(std::cin, input2);
        if (input1.size() == input2.size() &&
            isRotation(input1.c_str(), input2.c_str())) {
            std::cout << "rotation" << std::endl;
        } else {
            std::cout << "not rotation" << std::endl;
        }
        std::cout << std::endl;
    }
    return 0;
}
