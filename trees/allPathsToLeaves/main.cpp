#include <iostream>
#include <iomanip>
#include <cassert>
#include <ctime>
#include <cstdlib>
#include <vector>
#include <string>

// Print all paths from the root to each leaf

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
    void insertAux(TreeNode * parent, TreeNode * n) {
        assert(parent && n);
        if (rand() % 2) { // Left
            if (parent->left) {
                insertAux(parent->left, n);
            } else {
                parent->left = n;
            }
        } else { // Right
            if (parent->right) {
                insertAux(parent->right, n);
            } else {
                parent->right = n;
            }
        }
    }
    void insert(int v) {
        TreeNode * n = new TreeNode(v);
        if (!root) {
            root = n;
        } else {
            insertAux(root, n);
        }
    }
    void print(std::ostream & os, TreeNode * n, int indent) const {
        if (!n) { return; }
        print(os, n->right, indent + 5);
        os << std::setw(indent) << n->val << std::endl;
        print(os, n->left, indent + 5);
    }
    void findPaths(TreeNode * parent, std::vector<std::string> & result,
                   std::string currentPath) const {
        if (!parent) { return; }
        if (currentPath.empty()) {
            currentPath += std::to_string(parent->val);
        } else {
            currentPath += "->" + std::to_string(parent->val);
        }
        findPaths(parent->left, result, currentPath);
        findPaths(parent->right, result, currentPath);
        if (!parent->left && !parent->right) { // Is leaf
            if (!currentPath.empty()) { result.push_back(currentPath); }
        }
    }
    void printLeafPaths() const {
        std::vector<std::string> result;
        findPaths(root, result, "");
        for (auto path : result) {
            std::cout << path << std::endl;
        }
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
        t.insert(i);
    }
    std::cout << t << std::endl;
    t.printLeafPaths();
    return 0;
}
