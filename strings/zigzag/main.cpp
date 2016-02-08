#include <iostream>
#include <string>

// Given a string and a number of rows, print it out as a zig-zag

void printAsZigZag(const std::string & s, int cols) {
    int currIndex = 0;
    while (currIndex < s.size()) {
        // Print out a row
        for (int i = 0; i < cols; ++i) {
            if (currIndex >= s.size()) { std::cout << std::endl; return; }
            std::cout << s[currIndex];
            ++currIndex;
        }
        std::cout << std::endl;
        // Print out the diagonal
        for (int i = 0; i < cols - 2; ++i) {
            if (currIndex >= s.size()) { std::cout << std::endl; return; }
            std::cout << std::string(cols - (i + 2), ' ') << s[currIndex]
                      << std::endl;
            ++currIndex;
        }
    }
    std::cout << std::endl;
}

int main() {
    std::string s("ARMADILLO");
    for (int i = 1; i < 5; ++i) {
        std::cout << "Printing ARMADILLO with " << i << " columns.\n";
        printAsZigZag(s, i);
        std::cout << std::endl;
    }
    return 0;
}
