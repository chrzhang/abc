#include <iostream>
#include <iomanip>
#include <fstream>
#include <cctype>
#include <unordered_map>

// Count occurrences of words

int main() {
    std::ifstream inFile;
    inFile.open("input.txt");
    std::string word;
    std::unordered_map<std::string, unsigned> counts;
    while (inFile >> word) {
        std::string trimmedWord;
        for (auto it = word.begin(); it != word.end(); ++it) {
            if (((*it - 'a') >= 0 && (*it - 'a') <= 25) ||
                ((*it - 'A') >= 0 && (*it - 'A') <= 25)) {
                trimmedWord += tolower(*it);
            } else {
            }
        }
        if (!trimmedWord.empty()) {
            counts[trimmedWord]++;
        }
    }
    inFile.close();
    for (auto it = counts.begin(); it != counts.end(); ++it) {
        std::cout << "(" << it->first << " " << it->second
                  << ") ";
    }
    std::cout << std::endl;
    return 0;
}
