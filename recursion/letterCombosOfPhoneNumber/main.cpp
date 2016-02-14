#include <iostream>
#include <unordered_map>
#include <list>

// Find all letter combinations of a phone number

std::unordered_map<char, std::string> buttons =
    {{'2', "abc"}, {'3', "def"}, {'4', "ghi"}, {'5', "jkl"},
     {'6', "mno"}, {'7', "pqrs"}, {'8', "tuv"}, {'9', "wxyz"}};

void findLettersAux(std::list<std::string> & result,
                    const std::string & phoneNumber,
                    std::string strSoFar, int index) {
    if (index == phoneNumber.size()) { result.push_back(strSoFar); }
    for (auto c : buttons[phoneNumber[index]]) {
        std::string strSoFarCopy = strSoFar;
        strSoFarCopy.append(1, c);
        findLettersAux(result, phoneNumber, strSoFarCopy, index + 1);
    }
}

std::list<std::string> findLetterCombinations(const std::string &
                                                    phoneNumber) {
    std::list<std::string> result;
    if (phoneNumber.empty()) { return result; }
    findLettersAux(result, phoneNumber, "", 0);
    return result;
}

int main() {
    auto allStrs = findLetterCombinations("23");
    for (auto s : allStrs) {
        std::cout << s << std::endl;
    }
    return 0;
}
