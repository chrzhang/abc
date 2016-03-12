#include <iostream>
#include <string>
#include <unordered_set>
#include <vector>

// Find all ways to break a sentence into words

void wordBreakAux(const std::string & s,
                  const std::unordered_set<std::string> & wordDict,
                  std::vector<std::vector<std::string>> & results,
                  std::vector<std::string> currSol) {
    // Try adding a space after each char (even last, which is no spaces)
    if (s.empty()) { return; }
    for (auto it = next(s.begin()); ; ++it) {
        auto ss = std::string(s.begin(), it);
        if (wordDict.find(ss) != wordDict.end()) {
            currSol.push_back(ss);
            if (it == s.end()) {
                results.push_back(currSol);
                return;
            }
            wordBreakAux(std::string(it, s.end()), wordDict, results, currSol);
            currSol.pop_back();
        }
        if (it == s.end()) {
            return;
        }
    }
}

std::vector<std::string> wordBreak(std::string s,
                                   std::unordered_set<std::string> & wordDict) {
    std::vector<std::string> answer;
    std::vector<std::vector<std::string>> results;
    wordBreakAux(s, wordDict, results, std::vector<std::string>());
    for (auto v : results) {
        std::string cat;
        for (auto s : v) {
            cat += s + " ";
        }
        if (!cat.empty()) {
            cat.pop_back();
            answer.push_back(cat);
        }
    }
    return answer;
}

int main() {
    std::unordered_set<std::string> dict = { "cat", "cats", "and", "sand",
                                             "dog" };
    auto r = wordBreak("catsanddog", dict);
    for (auto s : r) {
        std::cout << s << std::endl;
    }
    return 0;
}
