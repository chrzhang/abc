#include <iostream>
#include <string>
#include <set>
#include <cassert>

// Given a sentence without whitespace, find a way to split the character so
// that the fewest numbers of characters are left as unknown words

int findSplit(const std::set<std::string> dict, const std::string & s,
              int begin, int current, std::set<int> & indices) {
    // Base case
    if (current + 1 == s.size()) {
        if (dict.find(s.substr(begin, current - begin + 1)) != dict.end()) {
            return 0;
        } else {
            return (current - begin + 1);
        }
    }
    // Either add a space after current or don't (pick better option)
    int numPrefixInvalidChars = 0;
    if (dict.find(s.substr(begin, current - begin + 1)) == dict.end()) {
        numPrefixInvalidChars += current - begin + 1;
    }
    int lenIfSplit = numPrefixInvalidChars + findSplit(dict, s, current + 1,
                                                       current + 1, indices);
    int lenIfNotSplit = findSplit(dict, s, begin, current + 1, indices);
    if (current && lenIfSplit < lenIfNotSplit) {
        indices.insert(current);
    }
    return (lenIfSplit < lenIfNotSplit ? lenIfSplit : lenIfNotSplit);
}

// Given the string without spaces, build the proper sentence
std::string sensicalSplit(const std::set<std::string> dict,
                          const std::string & s) {
    std::string str = s;
    std::set<int> indices; // Store indices where putting spaces yields solution
    findSplit(dict, s, 0, 0, indices);
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
