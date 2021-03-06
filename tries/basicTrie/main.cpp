#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <set>
#include <cassert>

// Implement basic trie building

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
            // Handle dupes
            for (auto it = parent->children.begin();
                 it != parent->children.end(); ++it) {
                if ((*it)->c == '\0') {
                    return;
                }
            }
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
    bool containStringAux(TrieNode * parent, const std::string & s,
                          const std::string::const_iterator sit) {
        if (sit == s.end()) {
            for (auto it = parent->children.begin();
                 it != parent->children.end(); ++it) {
                if ((*it)->c == '\0') {
                    return true;
                }
            }
            return false;
        }
        for (auto it = parent->children.begin(); it != parent->children.end();
             ++it) {
            if ((*it)->c == *sit) {
                return containStringAux(*it, s, std::next(sit, 1));
            }
        }
        return false;
    }
    bool contains(const std::string & s) {
        if (s.empty()) { return false; }
        return containStringAux(root, s, s.begin());
    }
    void printWithPrefix(const std::string & s, TrieNode * parent) {
        assert(parent);
        auto sCopy = s;
        sCopy += std::string(1, parent->c);
        if (parent->children.empty()) {
            std::cout << sCopy << std::endl;
        } else {
            for (auto it = parent->children.begin();
                 it != parent->children.end(); ++it) {
                printWithPrefix(sCopy, *it);
            }
        }
    }
    void autocompleteAux(TrieNode * parent, const std::string & s,
                         const std::string::const_iterator sit) {
        if (sit == s.end()) {
            for (auto it = parent->children.begin();
                 it != parent->children.end(); ++it) {
                printWithPrefix(s, *it);
            }
        }
        for (auto it = parent->children.begin(); it != parent->children.end();
             ++it) {
            if ((*it)->c == *sit) {
                autocompleteAux(*it, s, std::next(sit, 1));
            }
        }
    }
    void autocomplete(const std::string & s) { // Print strings with prefix s
        if (s.empty()) { return; }
        return autocompleteAux(root, s, s.begin());
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

void printAux(std::ostream & os, std::string strSoFar, TrieNode * n,
              int indent) {
    os << std::string(indent, ' ') << strSoFar + n->c
       << (n->children.empty() ? " <--" : "") << std::endl;
    for (auto it = n->children.begin(); it != n->children.end(); ++it) {
        printAux(os, strSoFar + n->c, *it, indent + 1);
    }
}

std::ostream & operator<<(std::ostream & os, const Trie & t) {
    for (auto it = t.root->children.begin(); it != t.root->children.end();
         ++it) {
        printAux(os, "", *it, 0);
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
        assert(t.contains(*it));
    }
    //std::cout << t << std::endl;
    while (true) {
        std::cout << "Enter prefix of word (Ctrl + C to quit): " << std::endl;
        std::cin >> currWord;
        std::cout << "Found:\n======\n";
        t.autocomplete(currWord);
        std::cout << std::endl;
    }
    return 0;
}
