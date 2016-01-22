#include <iostream>
#include <string>
#include <set>
#include <cassert>
#include <map>

// Given a sentence without whitespace, find a way to split the character so
// that the fewest numbers of characters are left as unknown words

int findSplit(const std::set<std::string> dict, const std::string & s,
              int begin, int current, std::set<int> & indices,
              std::map<std::pair<int, int>, int> & results) {
    auto seekOldResults = results.find(std::pair<int, int>(begin, current));
    if (seekOldResults != results.end()) {
        return seekOldResults->second;
    }
    // Base case
    if (current + 1 == s.size()) {
        if (dict.find(s.substr(begin, current - begin + 1)) != dict.end()) {
            results[std::pair<int, int>(begin, current)] = 0;
            return 0;
        } else {
            results[std::pair<int, int>(begin, current)] = current - begin + 1;
            return (current - begin + 1);
        }
    }
    // Either add a space after current or don't (pick better option)
    int numPrefixInvalidChars = 0;
    if (dict.find(s.substr(begin, current - begin + 1)) == dict.end()) {
        numPrefixInvalidChars += current - begin + 1;
    }
    int lenIfSplit = numPrefixInvalidChars + findSplit(dict, s, current + 1,
                                                       current + 1, indices,
                                                       results);
    int lenIfNotSplit = findSplit(dict, s, begin, current + 1, indices,
                                  results);
    if (current && lenIfSplit < lenIfNotSplit) {
        indices.insert(current);
    }
    results[std::pair<int, int>(begin, current)] = lenIfSplit < lenIfNotSplit ?
                                         lenIfSplit : lenIfNotSplit;
    return (lenIfSplit < lenIfNotSplit ? lenIfSplit : lenIfNotSplit);
}

// Given the string without spaces, build the proper sentence
std::string sensicalSplit(const std::set<std::string> dict,
                          const std::string & s) {
    std::string str = s;
    std::set<int> indices; // Store indices where putting spaces yields solution
    std::map<std::pair<int, int>, int> results; // Store old results (DP)
    findSplit(dict, s, 0, 0, indices, results);
    int offset = 0;
    for (auto index = indices.begin(); index != indices.end(); ++index) {
        str.insert(*index + 1 + offset, 1, ' ');
        ++offset;
    }
    return str;
}

int main() {
    std::set<std::string> dict;
    dict.insert("apple");
    dict.insert("bear");
    dict.insert("cat");
    auto sentence = sensicalSplit(dict, "catbearjohndoeapple");
    assert(0 == sentence.compare("cat bear johndoe apple"));
    std::cout << "Split 'catbearjohndoeapple' into '" << sentence << "'"
              << std::endl;
    return 0;
}
