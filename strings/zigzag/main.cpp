#include <iostream>
#include <vector>
#include <string>

// Given a string and a number of rows, print it out as a zig-zag

void printAsZigZag(const std::string & s, int cols) {
    size_t currIndex = 0;
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

void printCharsPerCol(const std::string & s, int cols) {
    std::vector<std::vector<char>> charsInEveryColumn;
    for (int i = 0; i < cols; ++i) {
        charsInEveryColumn.push_back(std::vector<char>());
    }
    size_t currIndex = 0;
    size_t c = 0;
    while (currIndex < s.size()) {
        // Print out a row
        for (int i = 0; i < cols; ++i) {
            if (currIndex >= s.size()) { break; }
            charsInEveryColumn[c].push_back(s[currIndex]);
            ++currIndex;
            ++c;
        }
        c = 0;
        // Print out the diagonal
        for (int i = 0; i < cols - 2; ++i) {
            if (currIndex >= s.size()) { break; }
            c = cols - (i + 2);
            charsInEveryColumn[c].push_back(s[currIndex]);
            ++currIndex;
        }
        c = 0;
    }
    std::vector<char> everything;
    for (auto colVec : charsInEveryColumn) {
        everything.insert(everything.end(), colVec.begin(), colVec.end());
    }
    std::cout << std::string(everything.begin(), everything.end()) << std::endl;
}

int main() {
    std::string s("ARMADILLO");
    for (int i = 1; i < 5; ++i) {
        std::cout << "Printing ARMADILLO with " << i << " columns.\n";
        printAsZigZag(s, i);
        std::cout << "\nPrinting string by going per column.\n";
        printCharsPerCol(s, i);
        std::cout << std::endl;
    }
    return 0;
}
