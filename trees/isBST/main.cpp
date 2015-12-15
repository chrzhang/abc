#include <iostream>
#include <cstdlib>
#include <ctime>
#include <assert.h>
#include <iomanip>
#include <limits.h>

#define NUM_NODES 10

// Determine if a binary tree is a binary search tree

struct TreeNode {
    int val;
    TreeNode * leftChild, * rightChild;
    TreeNode(int val) {
        this->val = val;
        leftChild = rightChild = nullptr;
    }
};

struct Tree {
    TreeNode * root;
    Tree() {
        root = nullptr;
    }
    void destroyHelper(TreeNode * parent) {
        if (parent == nullptr) { return; }
        destroyHelper(parent->leftChild);
        destroyHelper(parent->rightChild);
        delete parent;
    }
    ~Tree() {
        destroyHelper(root);
    }
    void insertHelper(TreeNode * parent, TreeNode * n) {
        assert(parent && n);
        switch (rand() % 2) {
            case 0: {
                if (parent->leftChild) {
                    insertHelper(parent->leftChild, n);
                } else {
                    parent->leftChild = n;
                }
                break;
            }
            case 1: {
                if (parent->rightChild) {
                    insertHelper(parent->rightChild, n);
                } else {
                    parent->rightChild = n;
                }
                break;
            }
        }
    }
    void insertRandomly(TreeNode * n) {
        if (!n) { return; }
        if (!root) {
            root = n;
        } else {
            insertHelper(root, n);
        }
    }
    void fill(int numNodes) {
        for (int i = 0; i < numNodes; ++i) {
            insertRandomly(new TreeNode(rand() % 100));
        }
    }
    bool isBSTAux(TreeNode * n, int floor, int ceiling) const {
        if (!n) { return true; }
        if (n->val < floor) { return false; }
        if (n->val >= ceiling) { return false; }
        if (n->leftChild) {
            if (n->val <= n->leftChild->val) {
                return false;
            }
            if (!isBSTAux(n->leftChild, floor, n->val)) {
                return false;
            }
        }
        if (n->rightChild) {
            if (n->val > n->rightChild->val) {
                return false;
            }
            if (!isBSTAux(n->rightChild, n->val, ceiling)) {
                return false;
            }
        }
        return true;
    }
    bool isBST() const {
        return isBSTAux(root, INT_MIN, INT_MAX);
    }
    void printAux(std::ostream & os, int indent, TreeNode * n) const {
        if (!n) { return; }
        printAux(os, indent + 5, n->rightChild);
        os << std::setw(indent) << n->val << std::endl;
        printAux(os, indent + 5, n->leftChild);
    }
    void print(std::ostream & os) const {
        printAux(os, 0, root);
    }
};

std::ostream & operator<<(std::ostream & os, const Tree & t) {
    t.print(os);
    return os;
}

int main() {
    srand(time(0));
    while (true) { // Keep generating trees until a BST is found
        Tree t;
        t.fill(NUM_NODES);
        if (t.isBST()) {
            std::cout << t << std::endl;
            break;
        }
    }
    return 0;
}
