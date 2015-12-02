#include <iostream>
#include <string>
#include <algorithm>
#include <unordered_map>

// Determine if two strings are permutations of each other

bool isPermutation0(const std::string & s1, const std::string & s2) {
    std::string s3 = s1;
    std::string s4 = s2;
    std::sort(s3.begin(), s3.end());
    std::sort(s4.begin(), s4.end());
    return s3 == s4;
}

bool isPermutation1(const std::string & s1, const std::string & s2) {
    std::unordered_map<char, unsigned> charCounter1, charCounter2;
    for (auto it1 = s1.begin(), it2 = s2.begin();
         it1 != s1.end() && it2 != s2.end();
         ++it1, ++it2) {
        charCounter1[*it1]++;
        charCounter2[*it2]++;
    }
    if (charCounter1.size() != charCounter2.size()) {
        return false;
    }
    for (auto it = charCounter1.begin(); it != charCounter1.end(); ++it) {
        if (it->second != charCounter2[it->first]) {
            return false;
        }
    }
    return true;
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
            !isPermutation1(inputLine1, inputLine2)) {
            std::cout << "not permutations of each other" << std::endl;
        } else {
            std::cout << "permutations of each other" << std::endl;
        }
        std::cout << std::endl;
    }
}
