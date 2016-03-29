#include <iostream>
#include <iomanip>
#include <cassert>

// Check if a binary tree is symmetric around its center

struct TreeNode {
    int val;
    TreeNode * left, * right;
    TreeNode(int v) : val(v), left(nullptr), right(nullptr) {}
};

struct Tree {
    TreeNode * root;
    Tree() : root(nullptr) {}
    void destroyAux(TreeNode * parent) {
        if (!parent) { return; }
        destroyAux(parent->left);
        destroyAux(parent->right);
        delete parent;
    }
    ~Tree() {
        destroyAux(root);
    }
    bool isSymmetricAux(TreeNode * n1, TreeNode * n2) const {
        if (!n1 && !n2) { return true; }
        if (n1 && !n2) { return false; }
        if (!n1 && n2) { return false; }
        if (n1->val != n2->val) { return false; }
        return isSymmetricAux(n1->left, n2->right) &&
               isSymmetricAux(n1->right, n2->left);
    }
    bool isSymmetric() const {
        if (!root) { return true; }
        return isSymmetricAux(root->left, root->right);
    };
    void print(std::ostream & os, TreeNode * n, int indent) const {
        if (!n) { return; }
        print(os, n->right, indent + 5);
        os << std::setw(indent) << n->val << "\n";
        print(os, n->left, indent + 5);
    }
};

std::ostream & operator<<(std::ostream & os, const Tree & t) {
    t.print(os, t.root, 0);
    return os;
}

int main() {
    {
        Tree t;
        t.root = new TreeNode(1);
        t.root->left = new TreeNode(2);
        t.root->right = new TreeNode(2);
        t.root->left->left = new TreeNode(3);
        t.root->left->right = new TreeNode(4);
        t.root->right->left = new TreeNode(4);
        t.root->right->right = new TreeNode(3);
        std::cout << t << std::endl;
        assert(t.isSymmetric());
    }
    {
        Tree t;
        t.root = new TreeNode(1);
        t.root->left = new TreeNode(2);
        t.root->right = new TreeNode(2);
        t.root->left->right = new TreeNode(3);
        t.root->right->right = new TreeNode(3);
        std::cout << t << std::endl;
        assert(!t.isSymmetric());
    }
    return 0;
}
