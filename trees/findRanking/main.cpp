#include <iostream>
#include <iomanip>
#include <vector>
#include <assert.h>
#include <cstdlib>
#include <ctime>
#include <algorithm>

// Given a stream of numbers, find a #'s ranking during input

struct TreeNode {
    int val;
    TreeNode * lc, * rc;
    TreeNode(int v) : val(v), lc(nullptr), rc(nullptr) {}
};

struct BST {
    TreeNode * root;
    BST() : root(nullptr) {}
    void destroyHelper(TreeNode * parent) {
        if (!parent) { return; }
        destroyHelper(parent->lc);
        destroyHelper(parent->rc);
        delete parent;
    }
    ~BST() {
        destroyHelper(root);
    }
    void insertHelper(TreeNode * parent, TreeNode * n) {
        assert(parent && n);
        if (n->val < parent->val) {
            if (parent->lc) {
                insertHelper(parent->lc, n);
            } else {
                parent->lc = n;
            }
        } else {
            if (parent->rc) {
                insertHelper(parent->rc, n);
            } else {
                parent->rc = n;
            }
        }
    }
    void insert(TreeNode * n) {
        if (!n) { return; }
        if (!root) {
            root = n;
        } else {
            insertHelper(root, n);
        }
    }
    void printAux(TreeNode * n, int indent, std::ostream & os) const {
        if (!n) { return; }
        printAux(n->rc, indent + 5, os);
        os << std::setw(indent) << n->val << std::endl;
        printAux(n->lc, indent + 5, os);
    }
    void print(std::ostream & os) const {
        printAux(root, 0, os);
    }
    void findRankOfAux(const int val, int & rank, TreeNode * n,
                       bool & found) const {
        if (!n || found) { return; }
        findRankOfAux(val, rank, n->lc, found);
        if (!found) { ++rank; }
        if (n->val == val) {
            found = true;
            return;
        }
        findRankOfAux(val, rank, n->rc, found);
    }
    int findRankOf(int val) const {
        int rank = 0;
        bool found = false;
        findRankOfAux(val, rank, root, found);
        if (rank == 0) { return -1; } // Never found
        return rank;
    }
    void inOrderPrintAux(TreeNode * n) const {
        if (!n) { return; }
        inOrderPrintAux(n->lc);
        std::cout << "[" << findRankOf(n->val) << "]: " << n->val << " ";
        inOrderPrintAux(n->rc);
    }
    void inOrderPrint() const {
        inOrderPrintAux(root);
        std::cout << std::endl;
    }
};

std::ostream & operator<<(std::ostream & os, const BST & bst) {
    bst.print(os);
    return os;
}

int main() {
    srand(time(0));
    BST bst;
    std::vector<int> v; // To test against
    for (int i = 0; i < 10; ++i) {
        auto randVal = rand() % 100;
        bst.insert(new TreeNode(randVal));
        v.push_back(randVal);
        std::sort(v.begin(), v.end());
        for (auto it = v.begin(); it != v.end(); ++it) {
            assert(v[bst.findRankOf(*it) - 1] == *it);
        }
    }
    return 0;
}
