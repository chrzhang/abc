#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <ctime>
#include <assert.h>

#define NUM_NODES1 10
#define NUM_NODES2 3

// Given two very large binary trees, decide if one is a subtree of the other

struct TreeNode {
    int val;
    TreeNode * leftChild, * rightChild;
    TreeNode(int val) {
        this->val = val;
        leftChild = rightChild = nullptr;
    }
};

bool treeEq(const TreeNode * n1, const TreeNode * n2) {
    if (!n1 && !n2) {
        return true;
    }
    if (!n1 && n2) {
        return false;
    }
    if (n1 && !n2) {
        return false;
    }
    assert(n1 && n2);
    if (n1->val != n2->val) { return false; }
    if (!treeEq(n1->leftChild, n2->leftChild)) {
        return false;
    }
    if (!treeEq(n1->rightChild, n2->rightChild)) {
        return false;
    }
    return true;
}

struct Tree {
    TreeNode * root;
    Tree() {
        root = nullptr;
    }
    void destroyHelper(TreeNode * n) {
        if (!n) { return; }
        destroyHelper(n->leftChild);
        destroyHelper(n->rightChild);
        delete n;
    }
    ~Tree() {
        destroyHelper(root);
    }
    void insertRandomlyHelper(TreeNode * parent, TreeNode * n) {
        assert(parent && n);
        if (rand() % 2) {
            if (parent->leftChild) {
                insertRandomlyHelper(parent->leftChild, n);
            } else {
                parent->leftChild = n;
            }
        } else {
            if (parent->rightChild) {
                insertRandomlyHelper(parent->rightChild, n);
            } else {
                parent->rightChild = n;
            }
        }
    }
    void insertRandomly(TreeNode * n) {
        if (!root) {
            root = n;
            return;
        }
        insertRandomlyHelper(root, n);
    }
    void printHelper(std::ostream & os, int indent, TreeNode * n) const {
        if (!n) { return; }
        printHelper(os, indent + 5, n->rightChild);
        os << std::setw(indent) << n->val << std::endl;
        printHelper(os, indent + 5, n->leftChild);
    }
    void print(std::ostream & os) const {
        printHelper(os, 0, root);
    }
    bool isSubtreeAux(TreeNode * currRoot, const TreeNode * otherRoot) const {
        if (!currRoot) { return false; }
        if (currRoot->val == otherRoot->val) {
            return treeEq(currRoot, otherRoot);
        }
        if (isSubtreeAux(currRoot->leftChild, otherRoot)) { return true; }
        if (isSubtreeAux(currRoot->rightChild, otherRoot)) { return true; }
        return false;
    }
    bool isSubtree(const Tree & otherTree) const {
        if (!otherTree.root) { return true; }
        return isSubtreeAux(root, otherTree.root);
    }
};

std::ostream & operator<<(std::ostream & os, const Tree & t) {
    t.print(os);
    return os;
}

int main() {
    srand(time(0));
    while (true) {
        Tree t1, t2;
        for (int i = 0; i < NUM_NODES1; ++i) {
            t1.insertRandomly(new TreeNode(1 + rand() % 10));
        }
        for (int i = 0; i < NUM_NODES2; ++i) {
            t2.insertRandomly(new TreeNode(1 + rand() % 10));
        }
        if (t1.isSubtree(t2)) {
            std::cout << "t1\n" << t1 << std::endl;
            std::cout << "t2\n" << t2 << std::endl;
            std::cout << "t2 is a subtree of t1.\n";
            break;
        }
    }
    return 0;
}
