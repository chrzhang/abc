#include <iostream>
#include <iomanip>
#include <assert.h>
#include <cstdlib>

#define NUM_NODES 40
#define TREE_INDENT 5

// Determine if a tree is balanced (height of subtrees are no more than 1 diff.)

struct Node {
    int val;
    Node * lchild;
    Node * rchild;
    Node(int val) {
        this->val = val;
        lchild = rchild = nullptr;
    }
};

struct Tree {
    Node * root;
    Tree() {
        root = nullptr;
    }
    void destroyHelper(Node * n) {
        if (!n) { return; }
        destroyHelper(n->lchild);
        destroyHelper(n->rchild);
        delete n;
    }
    ~Tree() {
        destroyHelper(root);
    }
    void insertRandomlyHelper(Node * parent, Node * n) {
        assert(n && parent);
        switch (rand() % 2) {
            case 0: { // Left
                if (!parent->lchild) {
                    parent->lchild = n;
                } else {
                    insertRandomlyHelper(parent->lchild, n);
                }
                break;
            }
            case 1: { // Right
                if (!parent->rchild) {
                    parent->rchild = n;
                } else {
                    insertRandomlyHelper(parent->rchild, n);
                }
                break;
            }
        }
    }
    void insertRandomly(Node * n) {
        if (!n) { return; }
        if (!root) {
            root = n;
            return;
        }
        insertRandomlyHelper(root, n);
    }
    void printHelper(int indent, Node * node) const {
        if (node) {
            printHelper(indent + 5, node->rchild);
            std::cout << std::setw(indent) << " " << node->val << std::endl;
            printHelper(indent + 5, node->lchild);
        }
    }
    void inOrderPrint(Node * n) const {
        if (!n) { return; }
        inOrderPrint(n->lchild);
        std::cout << n->val << " ";
        inOrderPrint(n->rchild);
    }
    void print() const {
        printHelper(0, root);
        std::cout << "in-order: ";
        inOrderPrint(root);
        std::cout << std::endl;
    }
    bool isBalanced() const {
        if (!root) return true;
        if (getHeightForBalancing(root) != -1) { return true; }
        return false;
    }
    int getHeightForBalancing(Node * parent) const {
        if (!parent) { return 0; }
        int leftHeight = getHeightForBalancing(parent->lchild);
        if (leftHeight == -1) { return -1; }
        int rightHeight = getHeightForBalancing(parent->rchild);
        if (rightHeight == -1) { return -1; }
        if (abs(leftHeight - rightHeight) > 1) { return -1; }
        return 1 + (leftHeight > rightHeight ? leftHeight : rightHeight);
    }
};

int main() { // Randomly generate trees until a balanced one is formed
    srand(time(0));
    bool foundBalancedTree = false;
    while (!foundBalancedTree) {
        Tree t;
        for (int i = 0; i < NUM_NODES; ++i) {
            t.insertRandomly(new Node(rand() % 100));
        }
        if (t.isBalanced()) {
            t.print();
            foundBalancedTree = true;
        }
    }
    return 0;
}
