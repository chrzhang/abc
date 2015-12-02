#include <iostream>
#include <string>
#include <algorithm>

// Determine if two strings are permutations of each other

bool isPermutation(const std::string & s1, const std::string & s2) {
    std::string s3 = s1;
    std::string s4 = s2;
    std::sort(s3.begin(), s3.end());
    std::sort(s4.begin(), s4.end());
    return s3 == s4;
}

int main() {
    while (true) {
        std::string inputLine1;
        std::string inputLine2;
        std::cout << "Enter the first string: ";
        std::getline(std::cin, inputLine1);
        std::cout << "Enter the second string: ";
        std::getline(std::cin, inputLine2);
        if (inputLine1.size() != inputLine2.size() ||
            !isPermutation(inputLine1, inputLine2)) {
            std::cout << "not permutations of each other" << std::endl;
        } else {
            std::cout << "permutations of each other" << std::endl;
        }
        std::cout << std::endl;
    }
}
