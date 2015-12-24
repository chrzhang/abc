#include <iostream>
#include <string>
#include <bitset>

// Find the next biggest, next smallest integer with the same amount of 1 bits

std::string toBin(int n) {
    return std::bitset<16>(n).to_string();
}

size_t numberOf1s(const std::string & s) {
    size_t a = 0;
    for (auto it = s.begin(); it != s.end(); ++it) {
        a += *it == '1' ? 1 : 0;
    }
    return a;
}

int main() {
    std::string input;
    std::cout << "Enter a number: ";
    getline(std::cin, input);
    int i = stoi(input);
    int numSetBits = numberOf1s(toBin(i));
    int j = i + 1;
    while (1) {
        auto s = toBin(j);
        if (numberOf1s(s) == numSetBits) {
            std::cout << "\tAfter: " << j << "(" << s << ")" << std::endl;
            break;
        }
        ++j;
    }
    int h = i - 1;
    while (1) {
        auto s = toBin(h);
        if (numberOf1s(s) == numSetBits) {
            std::cout << "\tBefore: " << h << "(" << s << ")" << std::endl;
            break;
        }
        --h;

    }
    return 0;
}
