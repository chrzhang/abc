#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <ctime>
#include <cassert>

// Flatten a binary tree to a list

struct TreeNode {
    int val;
    TreeNode * left;
    TreeNode * right;
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
    void insertRightAux(TreeNode * parent, TreeNode * n) {
        if (!(parent && n)) { return; }
        if (parent->right) {
            insertRightAux(parent->right, n);
        } else {
            parent->right = n;
        }
    }
    void insertLeftAux(TreeNode * parent, TreeNode * n) {
        if (!(parent && n)) { return; }
        if (parent->left) {
            insertLeftAux(parent->left, n);
        } else {
            parent->left = n;
        }
    }
    void insertRight(int val) {
        TreeNode * n = new TreeNode(val);
        if (!root) {
            root = n;
        } else {
            insertRightAux(root, n);
        }
    }
    void insertRandomAux(TreeNode * parent, TreeNode * n) {
        if (!(parent && n)) { return; }
        if (rand() % 2) {
            if (parent->left) {
                insertRandomAux(parent->left, n);
            } else {
                parent->left = n;
            }
        } else {
            if (parent->right) {
                insertRandomAux(parent->right, n);
            } else {
                parent->right = n;
            }
        }
    }
    void insertRandom(int val) {
        TreeNode * n = new TreeNode(val);
        if (!root) {
            root = n;
        } else {
            if (rand() % 2) {
                if (root->left) {
                    insertRandomAux(root->left, n);
                } else {
                    root->left = n;
                }
            } else {
                if (root->right) {
                    insertRandomAux(root->right, n);
                } else {
                    root->right = n;
                }
            }
        }
    }
    void print(std::ostream & os, TreeNode * n, int indent) const {
        if (!n) { return; }
        print(os, n->right, indent + 5);
        os << std::setw(indent) << n->val << std::endl;
        print(os, n->left, indent + 5);
    }
    void flattenAux2(TreeNode * & otherTreeRoot, TreeNode * root) {
        if (!root) { return; }
        auto temp1 = root->left;
        auto temp2 = root->right;
        root->left = root->right = nullptr;
        if (!otherTreeRoot) { otherTreeRoot = root; }
        else { insertRightAux(otherTreeRoot, root); }
        flattenAux2(otherTreeRoot, temp1);
        flattenAux2(otherTreeRoot, temp2);
    }
    void flattenAux1(TreeNode * root) {
        if (!root) { return; }
        TreeNode * otherRoot = nullptr;
        flattenAux2(otherRoot, root);
    }
    void flatten() {
        flattenAux1(root);
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
        t.insertRandom(i);
    }
    std::cout << t << std::endl;
    std::cout << "Flattened:\n";
    t.flatten();
    std::cout << t << std::endl;
    return 0;
}
