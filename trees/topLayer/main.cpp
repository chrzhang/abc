#include <iostream>
#include <iomanip>
#include <cassert>

// Print the top outermost layer of a tree (as if looking down from the top and
// the standard tree display is a cross-section)

struct TreeNode {
    int val;
    TreeNode * left;
    TreeNode * right;
    TreeNode(int v) : val(v), left(nullptr), right(nullptr) {}
};

struct Tree {
    TreeNode * root;
    Tree() : root(nullptr) {}
    void destroyAux(TreeNode * n) {
        if (!n) { return; }
        destroyAux(n->left);
        destroyAux(n->right);
        delete n;
    }
    ~Tree() {
        destroyAux(root);
    }
    void print(std::ostream & os, TreeNode * n, int indent) const {
        if (!n) { return; }
        print(os, n->right, indent + 5);
        os << std::setw(indent) << n->val << std::endl;
        print(os, n->left, indent + 5);
    }
};

void top_view_aux_left(TreeNode * root) {
    if (!root) { return; }
    top_view_aux_left(root->left);
    std::cout << root->val << " ";
}

void top_view_aux_right(TreeNode * root) {
    if (!root) { return; }
    std::cout << root->val << " ";
    top_view_aux_right(root->right);
}

void top_view(TreeNode * root) {
    if (!root) { return; }
    top_view_aux_left(root->left);
    std::cout << root->val << " ";
    top_view_aux_right(root->right);
    std::cout << std::endl;
}

std::ostream & operator<<(std::ostream & os, const Tree & t) {
    t.print(os, t.root, 0);
    return os;
}

int main() {
    TreeNode * n1 = new TreeNode(1);
    TreeNode * n2 = new TreeNode(2);
    TreeNode * n3 = new TreeNode(3);
    TreeNode * n4 = new TreeNode(4);
    TreeNode * n5 = new TreeNode(5);
    TreeNode * n6 = new TreeNode(6);
    TreeNode * n7 = new TreeNode(7);
    TreeNode * n8 = new TreeNode(8);
    TreeNode * n9 = new TreeNode(9);
    Tree t;
    t.root = n3;
    n3->left = n5;
    n3->right = n2;
    n5->left = n1;
    n5->right = n4;
    n1->right = n9;
    n2->left = n6;
    n2->right = n7;
    n7->left = n8;
    std::cout << t;
    top_view(t.root);
    return 0;
}
