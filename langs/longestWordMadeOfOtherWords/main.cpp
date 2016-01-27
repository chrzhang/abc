#include <iostream>
#include <fstream>
#include <set> // Fast look-up to see if a word is a word
#include <vector> // Sort words by length so first success can be the last
#include <string>
#include <algorithm>
#include <cassert>

// Find longest word made up of other words

bool sortByLength(const std::set<std::string>::const_iterator & it1,
                  const std::set<std::string>::const_iterator & it2) {
    return it1->size() > it2->size();
}

bool isMadeUpOfOtherWords(const std::string & s,
                          const std::set<std::string> & allWords) {
    if (s.empty()) { return false; }
    if (s.size() == 1) {
        return allWords.find(s) != allWords.end();
    }
    auto split = std::next(s.begin());
    while (split != s.end()) {
        auto before = std::string(s.begin(), split);
        auto after = std::string(split, s.end());
        auto seekBefore = allWords.find(before) != allWords.end();
        auto seekAfter = allWords.find(after) != allWords.end();
        if (seekBefore && seekAfter) {
            return true;
        }
        auto beforeRec = isMadeUpOfOtherWords(before, allWords);
        auto afterRec = isMadeUpOfOtherWords(after, allWords);
        if (seekBefore && afterRec) {
            return true; }
        if (seekAfter && beforeRec) {
            return true; }
        if (beforeRec && afterRec) {
            return true; }
        split = std::next(split);
    }
    return false;
}

int main() {
    std::set<std::string> allWords;
    std::ifstream fileIn;
    fileIn.open("input.txt");
    std::string currWord;
    while (fileIn >> currWord) {
        allWords.insert(currWord);
    }
    fileIn.close();
    std::vector<std::set<std::string>::const_iterator> wordIterators;
    for (auto it = allWords.begin(); it != allWords.end(); ++it) {
        wordIterators.push_back(it);
    }
    std::sort(wordIterators.begin(), wordIterators.end(), sortByLength);
    std::string result;
    for (auto it = wordIterators.begin(); it != wordIterators.end(); ++it) {
        if (isMadeUpOfOtherWords(**it, allWords)) {
            result = **it;
            break;
        }
        break;
    }
    if (result.empty()) {
        std::cout << "Could not find a word made of other words.\n";
    } else {
        std::cout << result << " is made of other words.\n";
    }
    return 0;
}
