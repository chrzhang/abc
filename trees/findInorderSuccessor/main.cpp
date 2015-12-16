#include <iostream>
#include <iomanip>
#include <assert.h>
#include <ctime>
#include <cstdlib>
#include <vector>
#include <algorithm> // To shuffle an ordered set of #s

#define NUM_NODES 20
#define NUM_ITERATIONS 100000

// Find the in-order successor to a given node in a binary search tree

struct TreeNode {
    int val;
    TreeNode * leftChild, * rightChild, * parent;
    TreeNode(int val) {
        this->val = val;
        leftChild = rightChild = parent = nullptr;
    }
};

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
    void inOrderTestAux(TreeNode * n) const {
        if (!n) { return; }
        inOrderTestAux(n->leftChild);
        TreeNode * succ = findInorderSuccessor(n);
        if (succ) {
            assert(succ->val == 1 + n->val);
        } else {
            assert(n->val == NUM_NODES - 1);
        }
        inOrderTestAux(n->rightChild);
    }
    void inOrderTest() const {
        inOrderTestAux(root);
    }
    void insertHelper(TreeNode * parent, TreeNode * n) {
        assert(parent && n);
        if (n->val < parent->val) { // Left
            if (parent->leftChild) {
                insertHelper(parent->leftChild, n);
            } else {
                parent->leftChild = n;
                n->parent = parent;
            }
        } else { // Right
            if (parent->rightChild) {
                insertHelper(parent->rightChild, n);
            } else {
                parent->rightChild = n;
                n->parent = parent;
            }
        }
    }
    void insertBST(TreeNode * n) {
        if (!n) { return; }
        if (!root) {
            root = n;
        } else {
            insertHelper(root, n);
        }
    }
    void printHelper(std::ostream & os, int indent, TreeNode * n) const {
        if (!n) { return; }
        printHelper(os, indent + 5, n->rightChild);
        os << std::setw(indent) << n->val << std::endl;
        printHelper(os, indent + 5, n->leftChild);
    }
    void print(std::ostream & os ) const {
        printHelper(os, 0, root);
    }
    bool isLeftChild(TreeNode * n, TreeNode * parent) const {
        assert(n && parent);
        return (parent->leftChild == n);
    }
    TreeNode * findInorderSuccessor(TreeNode * n) const {
        // Either the leftmost child of the right child subtree
        if (n->rightChild) {
            TreeNode * temp = n->rightChild;
            while (temp->leftChild) {
                temp = temp->leftChild;
            }
            return temp;
        } else { // Or the parent that most recently went left to reach node
            TreeNode * child = n;
            TreeNode * parent = n->parent;
            while (child && parent && !isLeftChild(child, parent)) {
                child = parent;
                parent = child->parent;
            }
            if (!parent) { return nullptr; }
            return parent;
        }
    }
};

std::ostream & operator<<(std::ostream & os, const Tree & t) {
    t.print(os);
    return os;
}

int randomGen(int max) {
    return rand() % max;
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        Tree t;
        std::vector<int> v;
        for (int i = 0; i < NUM_NODES; ++i) {
            v.push_back(i);
        }
        std::random_shuffle(v.begin(), v.end(), randomGen);
        for (int i = 0; i < NUM_NODES; ++i) {
            t.insertBST(new TreeNode(v[i]));
        }
        t.inOrderTest();
    }
    return 0;
}
