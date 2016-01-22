#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <unordered_map>

// Sort characters by the amount of times they occur in a string

bool compareChar(const char & c1, const char & c2,
                 std::unordered_map<char, int> & counts) {
    return counts[c1] > counts[c2];
}

int main() {
    std::unordered_map<char, int> counts;
    std::ifstream fileIn;
    fileIn.open("input.txt");
    std::string currLine;
    while (getline(fileIn, currLine)) {
        for (auto it = currLine.begin(); it != currLine.end(); ++it) {
            counts[*it]++;
        }
    }
    fileIn.close();
    std::string allChars;
    for (auto it = counts.begin(); it != counts.end(); ++it) {
        std::cout << it->first << ": " << it->second << std::endl;
        allChars.append(1, it->first);
    }
    std::sort(allChars.begin(), allChars.end(),
              std::bind(compareChar, std::placeholders::_1,
                        std::placeholders::_2, counts));
    std::cout << allChars << std::endl;
    return 0;
}
