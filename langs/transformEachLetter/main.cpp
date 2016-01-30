#include <iostream>
#include <cassert>
#include <set>
#include <string>
#include <vector>

// Transform one string to another string of the same size by transforming one
// letter at a time provided each intermediate string is a word

// Check if two strings are within one letter edit from another
// Return index of difference found
int isOneEditAway(const std::string & s1, const std::string & s2) {
    if (s1.size() != s2.size()) { return -1; }
    if (s1.empty()) { return -1; }
    auto it1 = s1.begin();
    auto it2 = s2.begin();
    int foundDifferenceAt = -1;
    for (int i = 0; i < s1.size(); ++i) {
        assert(it1 != s1.end());
        assert(it2 != s2.end());
        if (*it1 != *it2) {
            if (foundDifferenceAt != -1) { return -1; }
            foundDifferenceAt = i;
        }
        it1 = std::next(it1);
        it2 = std::next(it2);
    }
    return foundDifferenceAt;
}

void testIsOneEditAway() {
    assert(1 == isOneEditAway("me", "my"));
    assert(0 == isOneEditAway("cat", "bat"));
    assert(-1 == isOneEditAway("rat", "car"));
    assert(-1 == isOneEditAway("foo", "bar"));
    assert(-1 == isOneEditAway("a", "a"));
    assert(-1 == isOneEditAway("hello", "hello"));
}

void findTransformations(const std::string & s1, const std::string & s2,
                         const std::set<std::string> & allWords,
                         std::vector<bool> & visited) {
    //std::cout << "s1: " << s1 << " s2: " << s2 << std::endl;
    std::cout << s1 << std::endl;
    auto seek1 = allWords.find(s1);
    auto seek2 = allWords.find(s2);
    assert(seek1 != allWords.end() && seek2 != allWords.end());
    if (s1.compare(s2) == 0) { return; }
    visited[std::distance(allWords.begin(), seek1)] = true;
    for (auto it = allWords.begin(); it != allWords.end(); ++it) {
        auto word = *it;
        if (visited[std::distance(allWords.begin(), it)]) { continue; }
        auto indexOfDifference = isOneEditAway(word, s1);
        if (-1 != indexOfDifference) {
            findTransformations(word, s2, allWords, visited);
        }
    }
}

int main() {
   testIsOneEditAway();
   std::set<std::string> allWords;
   allWords.insert("foo");
   allWords.insert("boo");
   allWords.insert("bor");
   allWords.insert("bar");
   allWords.insert("baz");
   std::vector<bool> visited(allWords.size(), false);
   findTransformations("foo", "baz", allWords, visited);
   return 0;
}
