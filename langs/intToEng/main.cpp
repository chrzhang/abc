#include <iostream>
#include <iomanip>
#include <unordered_map>
#include <string>
#include <cstdlib>
#include <ctime>

#define N 100

// Convert numbers to English

std::string toEng(int r) {
    std::unordered_map<char, std::string> digits ({
        {'0', "zero"},
        {'1', "one"},
        {'2', "two"},
        {'3', "three"},
        {'4', "four"},
        {'5', "five"},
        {'6', "six"},
        {'7', "seven"},
        {'8', "eight"},
        {'9', "nine"}
    });
    std::unordered_map<char, std::string> teens ({
        {'0', "ten"},
        {'1', "eleven"},
        {'2', "twelve"},
        {'3', "thirteen"},
        {'4', "fourteen"},
        {'5', "fifteen"},
        {'6', "sixteen"},
        {'7', "seventeen"},
        {'8', "eighteen"},
        {'9', "nineteen"}
    });
    std::unordered_map<char, std::string> tens ({
        {'2', "twenty"},
        {'3', "thirty"},
        {'4', "forty"},
        {'5', "fifty"},
        {'6', "sixty"},
        {'7', "seventy"},
        {'8', "eighty"},
        {'9', "ninety"}
    });
    std::unordered_map<int, std::string> powersOfTen ({
        {0, ""},
        {3, "thousand"},
        {6, "million"},
        {9, "billion"}
    });
    std::string result;
    std::string num = std::to_string(r);
    while (num.size() % 3) {
        num = "0" + num;
    }
    for (auto it = num.begin(); it != num.end(); ++it) {
        // Hundreds
        if (*it != '0') {
            result += digits[*it] + " hundred ";
        }
        ++it;
        bool ignoreOnes = false;
        // Tens
        if (*it != '0') {
            switch (*it) {
                case '1': // Special case of teens
                    result += teens[*(it + 1)] + " ";
                    ignoreOnes = true;
                    break;
                default:
                    result += tens[*it] + " ";
                    break;
            }
        }
        ++it;
        // Ones
        if (*it != '0' && !ignoreOnes) {
            result += digits[*it] + " ";
        }
        // Power of ten
        result += powersOfTen[num.size() - (it - num.begin() + 1)] + " ";
    }
    if (0 == result.compare(" ")) {
        return "zero";
    }
    return result;
}

int main() {
    srand(time(0));
    for (int i = 0; i < N; ++i) {
        auto r = rand() % 10000000;
        std::cout << std::setw(10) << r << " " << toEng(r) << std::endl;
    }
    return 0;
}
