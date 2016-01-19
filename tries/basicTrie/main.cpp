#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <set>
#include <cassert>

struct TrieNode {
    char c;
    std::vector<TrieNode *> children;
    TrieNode(char c) : c(c) {}
};

struct Trie {
    TrieNode * root; // Special node does not hold any char
    Trie() : root(new TrieNode(0)) {}
    void destroy(TrieNode * parent) {
        assert(parent);
        for (auto it = parent->children.begin(); it != parent->children.end();
             ++it) {
            destroy(*it);
        }
        delete parent;
    }
    ~Trie() {
        destroy(root);
    }
    void insertStringAux(TrieNode * parent, const std::string & s,
                         const std::string::const_iterator sit) {
        if (sit == s.end()) {
            auto end = new TrieNode('\0'); // To separate subwords 'hi' vs 'hit'
            parent->children.push_back(end);
            return;
        }
        for (auto it = parent->children.begin(); it != parent->children.end();
             ++it) {
            if ((*it)->c == *sit) { // Already exists
                insertStringAux(*it, s, std::next(sit, 1));
                return;
            }
        }
        auto newChild = new TrieNode(*sit);
        parent->children.push_back(newChild);
        insertStringAux(newChild, s, std::next(sit, 1));
    }
    void insertString(const std::string & s) {
        if (s.empty()) { return; }
        insertStringAux(root, s, s.begin());
    }
    void getAllStringsAux(std::set<std::string> & dict, std::string strSoFar,
                          TrieNode * n) const {
        if (n->children.empty()) {
            dict.insert(strSoFar + n->c);
        }
        for (auto it = n->children.begin(); it != n->children.end(); ++it) {
            getAllStringsAux(dict, strSoFar + n->c, *it);
        }
    }
    std::set<std::string> getAllStrings() const {
        std::set<std::string> dict;
        for (auto it = root->children.begin(); it != root->children.end();
             ++it) {
            getAllStringsAux(dict, "", *it);
        }
        return dict;
    }
};

void printAux(std::ostream & os, std::string strSoFar, TrieNode * n) {
    if (n->children.empty()) {
        os << strSoFar + n->c << std::endl;
    }
    for (auto it = n->children.begin(); it != n->children.end(); ++it) {
        printAux(os, strSoFar + n->c, *it);
    }
}

std::ostream & operator<<(std::ostream & os, const Trie & t) {
    for (auto it = t.root->children.begin(); it != t.root->children.end();
         ++it) {
        printAux(os, "", *it);
    }
    return os;
}

int main() {
    std::ifstream fileIn;
    fileIn.open("/usr/share/dict/words");
    std::set<std::string> dict; // To compare against for testing
    Trie t;
    std::string currWord;
    while (fileIn >> currWord) {
        dict.insert(currWord);
        t.insertString(currWord);
    }
    fileIn.close();
    auto tdict = t.getAllStrings();
    for (auto it = dict.begin(); it != dict.end(); ++it) {
        if (tdict.find((*it) + '\0') == tdict.end()) {
            std::cout << *it << " could not be found.\n";
            assert(false);
        }
    }
    return 0;
}
