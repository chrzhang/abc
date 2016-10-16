#include <iostream>
#include <string>
#include <cassert>
#include <algorithm>

// Replace all target strings in a tag with a numeral
// Tags are bound by < and > and target strings match in a case insensitive
// way so that aBc matches ABC and abc alike

struct character_eq {
    bool operator()(char c1, char c2) {
        return std::tolower(c1) == std::tolower(c2);
    }
};

// std::string::find matches in a case sensitive way but std::search permits
// custom comparators to be used
int find_case_insensitive(const std::string & str,
                          const std::string & needle,
                          size_t start_index) {
    auto result = std::search(str.begin() + start_index, str.end(),
                              needle.begin(), needle.end(),
                              character_eq());
    if (result != str.end()) {
        return result - str.begin();
    } else {
        return -1;
    }
}

void replaceTag(const std::string & tagstring,
                const int code,
                std::string & toParse) {
    std::string result;
    for (size_t i = 0; i < toParse.size();) {
        result += toParse[i];
        if (toParse[i] == '<') {
            bool foundClosingTag = false;
            for (size_t j = i; !foundClosingTag; ++j) {
                if (toParse[j] == '>') {
                    // Replace all tagstring between begin and end with code
                    std::string substring = toParse.substr(i + 1, j - i - 1);
                    size_t sought =
                        find_case_insensitive(substring, tagstring, 0);
                    while (sought != std::string::npos) {
                        substring.replace(sought, tagstring.size(),
                                          std::to_string(code));
                        sought =
                            find_case_insensitive(substring, tagstring,
                                                  sought +
                                                  std::to_string(code).size());
                    }
                    result += substring;
                    foundClosingTag = true;
                    i = j;
                }
            }
        } else {
            i += 1;
        }
    }
    toParse = result;
}

void runTests() {
    std::string tagstring, toParse, result;
    int code;

    tagstring = "BODY";
    code = 10;
    toParse = "<><BODY garbage>body</BODY>";
    replaceTag(tagstring, code, toParse);
    result = "<><10 garbage>body</10>";
    assert(toParse == result);

    tagstring = "aBc";
    code = 923;
    toParse = "<dont replace this>abcabc<abcabcde>";
    replaceTag(tagstring, code, toParse);
    result = "<dont replace this>abcabc<923923de>";
    assert(toParse == result);

    tagstring = "table";
    code = 1;
    toParse = "<ta>bLe<TaBle width=100></table></ta>";
    replaceTag(tagstring, code, toParse);
    result = "<ta>bLe<1 width=100></1></ta>";
    assert(toParse == result);

    tagstring = "replace";
    code = 323;
    toParse = "nothing inside";
    replaceTag(tagstring, code, toParse);
    result = "nothing inside";
    assert(toParse == result);
}

int main() {
    runTests();
    return 0;
}
