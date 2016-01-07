#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <ctime>
#include <cstdlib>
#include <fstream>
#include <set>

#define NUM_STRINGS 100

// Sort strings so that anagrams will be adjacent

bool anagramCompare(const std::string & s1, const std::string & s2) {
    if (s1.compare(s2) == 0) { return false; }
    auto copyOfS1 = s1;
    auto copyOfS2 = s2;
    std::sort(copyOfS1.begin(), copyOfS1.end());
    std::sort(copyOfS2.begin(), copyOfS2.end());
    auto cmp = copyOfS1.compare(copyOfS2);
    if (cmp == 0) {
        return false;
    } else {
        return cmp < 0;
    }
}

int main() {
    srand(time(0));
    std::vector<std::string> allStrings;
    std::ifstream fileIn;
    fileIn.open("input.txt");
    std::string currWord;
    while (fileIn >> currWord) {
        allStrings.push_back(currWord);
    }
    fileIn.close();
    std::sort(allStrings.begin(), allStrings.end(), anagramCompare);
    std::set<std::string> table; // To print results nicely
    for (auto it = allStrings.begin(); it != allStrings.end(); ++it) {
        auto copyOfString = *it;
        std::sort(copyOfString.begin(), copyOfString.end());
        if (table.find(copyOfString) == table.end()) {
            std::cout << "| ";
            table.insert(copyOfString);
        }
        std::cout << *it << " ";
    }
    std::cout << std::endl;
    return 0;
}
