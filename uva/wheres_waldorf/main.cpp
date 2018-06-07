#include <iostream>
#include <cassert>
#include <set>
#include <fstream>
#include <vector>
#include <cctype>

using namespace std;

struct Node {
    char letter;
    mutable int row;
    mutable int col;
    mutable set<Node> children;
    Node(const char letter, const int row, const int col)
    : letter(tolower(letter)), row(row), col(col) {
    }
    bool operator<(const Node & rhs) const {
        return letter < rhs.letter;
    }
};

ostream & operator<<(ostream & os, const Node & n) {
    os << n.letter << "@" << n.row << "," << n.col;
    return os;
}

string makeString(const int rowDelta, const int colDelta,
                  const int row, const int col,
                  const vector<string> & allLines) {
    string result;
    for (int r = row, c = col;
         (r >= 0 && r < allLines.size()) && (c >= 0 && c < allLines[r].size());
         r += rowDelta, c += colDelta) {
        result += allLines[r][c];
    }
    return result;
}

struct Trie {
    Node root;
    Trie()
    : root(0, -1, -1) {
    }
    Trie(const vector<string> & allLines)
    : root(0, -1, -1) {
        for (size_t row = 0; row < allLines.size(); ++row) {
            for (size_t col = 0; col < allLines[0].size(); ++col) {
                for (int rowDiff = -1; rowDiff <= 1; rowDiff++) {
                    for (int colDiff = -1; colDiff <= 1; colDiff++) {
                        if (rowDiff == 0 && colDiff == 0) continue;
                        const string dirString = makeString(rowDiff, colDiff, row, col, allLines);
                        addWord(dirString, row, col);
                    }
                }
            }
        }
    }
    void addWordAux(const string & s,
                    const string::const_iterator cit,
                    const Node * currParent,
                    const int row, const int col) {
        if (cit == s.end()) {
            return;
        }
        const char currC = *cit;
        const Node currCNode = Node(currC, row, col);
        auto got = currParent->children.insert(currCNode);
        if (got.first->row > row ||
            (got.first->row == row && got.first->col > col)) {
            got.first->row = row;
            got.first->col = col;
        }
        addWordAux(s, next(cit), &(*got.first), row, col);
    }
    void addWord(const string & s, const int row, const int col) {
        assert(row >= 0 && col >= 0 && !s.empty());
        addWordAux(s, s.begin(), &root, row, col);
    }
    pair<int, int> findWordAux(const string & s,
                               const string::const_iterator & cit,
                               const Node & currParent) const {
        auto seek = currParent.children.find(Node(*cit, -1, -1));
        if (seek == currParent.children.end()) {
            return {-1, -1};
        }
        if (next(cit) == s.end()) { return {seek->row, seek->col};
        }
        return findWordAux(s, next(cit), *seek);
    }
    pair<int, int> findWord(const string & sRaw) const {
        string s = sRaw;
        for (auto & c : s) {
            c = tolower(c);
        }
        if (s.empty()) {
            return {-1, -1};
        }
        auto result = findWordAux(s, s.begin(), root);
        if (result == make_pair(-1, -1)) {
            return result;
        }
        return {result.first + 1, result.second + 1};
    }
};

void printNode(ostream & os, const Node & n, const int indent) {
    os << string(indent, ' ') << n << endl;
    for (const auto & c : n.children) {
        printNode(os, c, indent + 2);
    }
}

ostream & operator<<(ostream & os, const Trie & t) {
    printNode(os, t.root, 0);
    return os;
}

int main() {
    ifstream inFile("input.txt");
    int numRows, numCols;
    inFile >> numRows;
    inFile >> numCols;
    vector<string> allLines;
    string currLine;
    getline(inFile, currLine);
    for (size_t rowi = 0; rowi < numRows; ++rowi) {
        getline(inFile, currLine);
        assert(currLine.size() == numCols);
        allLines.push_back(currLine);
    }
    const Trie t(allLines);
    assert(make_pair(2, 5) == t.findWord("Waldorf"));
    assert(make_pair(2, 3) == t.findWord("Bambi"));
    assert(make_pair(1, 2) == t.findWord("Betty"));
    assert(make_pair(7, 8) == t.findWord("Dagbert"));
}
