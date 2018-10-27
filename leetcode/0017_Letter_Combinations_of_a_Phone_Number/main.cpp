class Solution {
    unordered_map<char, string> buttons = 
        {{'2', "abc"}, {'3', "def"}, {'4', "ghi"}, {'5', "jkl"},
         {'6', "mno"}, {'7', "pqrs"}, {'8', "tuv"}, {'9', "wxyz"}};

    void findLettersAux(std::vector<std::string> & result,
                        const std::string & phoneNumber,
                        std::string strSoFar, int index) {
        if (index == phoneNumber.size()) { result.push_back(strSoFar); }
        for (auto c : buttons[phoneNumber[index]]) {
            std::string strSoFarCopy = strSoFar;
            strSoFarCopy.append(1, c);
            findLettersAux(result, phoneNumber, strSoFarCopy, index + 1);
        }
    }

public:
    vector<string> letterCombinations(string phoneNumber) {
          std::vector<std::string> result;
          if (phoneNumber.empty()) { return result; }
          findLettersAux(result, phoneNumber, "", 0);
          return result;
    }
};