#include <iostream>
#include <map>
#include <string>
#include <cassert>

int fromRoman(const std::string & s) {
    std::map<char, int> romanNums =
        {{'I', 1}, {'V', 5}, {'X', 10}, {'L', 50}, {'C', 100}, {'D', 500},
         {'M', 1000}};
    int currAmt = 0;
    for (auto it = s.begin(); it != s.end(); ++it) {
        if (romanNums.find(*it) == romanNums.end()) {
            std::cout << "Could not find " << *it << " as a roman numeral.\n";
            return -1;
        }
        auto after = std::next(it);
        if (after != s.end() && romanNums[*after] > romanNums[*it]) {
            if (romanNums.find(*after) == romanNums.end()) {
                std::cout << "Could not find " << *after
                          << " as a roman numeral.\n";
                return -1;
            }
            currAmt  -= romanNums[*it];
            currAmt += romanNums[*after];
            ++it; // Must get incremented twice
            continue;
        }
        currAmt += romanNums[*it];
    }
    return currAmt;
}

int main() {
    assert(1 == fromRoman("I"));
    assert(5 == fromRoman("V"));
    assert(10 == fromRoman("X"));
    assert(50 == fromRoman("L"));
    assert(100 == fromRoman("C"));
    assert(500 == fromRoman("D"));
    assert(1000 == fromRoman("M"));
    assert(1954 == fromRoman("MCMLIV"));
    assert(1990 == fromRoman("MCMXC"));
    assert(2014 == fromRoman("MMXIV"));
    return 0;
}
