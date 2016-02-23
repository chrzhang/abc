#include <iostream>
#include <iomanip>
#include <cassert>

// Find the maximum depth of a binary tree

struct TreeNode {
    int val;
    TreeNode * left;
    TreeNode * right;
    TreeNode(int v) : val(v), left(nullptr), right(nullptr) {}
};

int max(int a, int b) {
    return a > b ? a : b;
}

struct Tree {
    TreeNode * root;
    Tree() : root(nullptr) {}
    void destroyHelper(TreeNode * n) {
        if (!n) { return; }
        destroyHelper(n->left);
        destroyHelper(n->right);
        delete n;
    }
    ~Tree() {
        destroyHelper(root);
    }
    void addAux(TreeNode * parent, TreeNode * n) {
        assert(parent && n);
        if (n->val < parent->val) {
            if (parent->left) {
                addAux(parent->left, n);
            } else {
                parent->left = n;
            }
        } else {
            if (parent->right) {
                addAux(parent->right, n);
            } else {
                parent->right = n;
            }
        }
    }
    void add(int val) {
        TreeNode * n = new TreeNode(val);
        if (!root) {
            root = n;
        } else {
            addAux(root, n);
        }
    }
    void print(std::ostream & os, TreeNode * n, int indent) const {
        if (!n) { return; }
        print(os, n->right, indent + 5);
        os << std::setw(indent) << n->val << std::endl;
        print(os, n->left, indent + 5);
    }
    int getMaxDepthAux(TreeNode * n) const {
        if (!n) { return 0; }
        return max(1 + getMaxDepthAux(n->left), 1 + getMaxDepthAux(n->right));
    }
    int getMaxDepth() const {
        return getMaxDepthAux(root);
    }
};

std::ostream & operator<<(std::ostream & os, const Tree & t) {
    t.print(os, t.root, 0);
    return os;
}

int main() {
    srand(time(0));
    Tree t;
    for (int i = 0; i < 10; ++i) {
        t.add(rand() % 5);
    }
    std::cout << t << std::endl;
    std::cout << "Depth: " << t.getMaxDepth() << std::endl;
    {
        Tree t;
        for (int i = 0; i < 10; ++i) {
            t.add(i);
        }
        assert(t.getMaxDepth() == 10);
    }
    return 0;
}
