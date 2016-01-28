#include <iostream>
#include <vector>
#include <string>
#include <cassert>

// Implement a basic suffix tree

struct TrieNode {
    char c;
    std::vector<TrieNode *> children;
    TrieNode(char c) : c(c) {}
};

struct SuffixTree {
    TrieNode * root;
    SuffixTree(const std::string & s) : root(new TrieNode(-1)) {
        insertWord(s);
    }
    void destroy(TrieNode * n) {
        for (auto it = n->children.begin(); it != n->children.end(); ++it) {
            destroy(*it);
        }
        delete n;
    }
    ~SuffixTree() {
        destroy(root);
    }
    TrieNode * getNodeThatMatches(char c, std::vector<TrieNode *> & children,
                                  bool createNodeIfNecessary = true) {
        for (auto it = children.begin(); it != children.end(); ++it) {
            if ((*it)->c == c) { return *it; }
        }
        if (createNodeIfNecessary) {
            auto newNode = new TrieNode(c);
            children.push_back(newNode);
            return newNode;
        } else {
            return nullptr;
        }
    }
    void insertWord(const std::string & s) {
        if (s.empty()) { return; }
        auto currentNode = root;
        for (auto sit = s.begin(); sit != s.end(); ++sit) {
            currentNode = getNodeThatMatches(*sit, currentNode->children);
        }
        auto sit = s.begin();
        while (std::next(sit) != s.end()) {
            auto suffix = std::string(std::next(sit), s.end());
            insertWord(suffix);
            sit = std::next(sit);
        }
    }
    bool isSubstring(const std::string & s) {
        if (s.empty()) { return true; }
        auto currentNode = root;
        for (auto sit = s.begin(); sit != s.end(); ++sit) {
            currentNode = getNodeThatMatches(*sit, currentNode->children,
                                             false);
            if (!currentNode) { return false; }
        }
        return true;
    }
    void print(std::ostream & os, int indent, TrieNode * n) const {
        if (n != root) {
            os << std::string(indent, '-') << n->c << std::endl;
        }
        for (auto it = n->children.begin(); it != n->children.end(); ++it) {
            print(os, indent + 2, *it);
        }
    }
};

std::ostream & operator<<(std::ostream & os, const SuffixTree & t) {
    t.print(os, 0, t.root);
    return os;
}

int main() {
    SuffixTree t("aardvark");
    assert(t.isSubstring("aardvark"));
    assert(t.isSubstring("ardvark"));
    assert(t.isSubstring("rdvark"));
    assert(t.isSubstring("dvark"));
    assert(t.isSubstring("vark"));
    assert(t.isSubstring("ark"));
    assert(t.isSubstring("rk"));
    assert(t.isSubstring("k"));
    assert(t.isSubstring(""));
    assert(t.isSubstring("aardvar"));
    assert(t.isSubstring("aardva"));
    assert(t.isSubstring("aardv"));
    assert(t.isSubstring("aard"));
    assert(t.isSubstring("aar"));
    assert(t.isSubstring("aa"));
    assert(t.isSubstring("a"));
    assert(t.isSubstring("ardvar"));
    assert(t.isSubstring("rdva"));
    assert(t.isSubstring("a"));
    assert(t.isSubstring("r"));
    assert(t.isSubstring("d"));
    assert(t.isSubstring("v"));
    assert(t.isSubstring("k"));
    std::cout << t << std::endl;
    return 0;
}
