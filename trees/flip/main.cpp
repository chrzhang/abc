#include <iostream>
#include <iomanip>
#include <cassert>
// Flip a binary tree

void swap(int & a, int & b) {
    int temp = a;
    a = b;
    b = temp;
}

struct TreeNode {
    int val;
    TreeNode * leftChild;
    TreeNode * rightChild;
    TreeNode(int val) : val(val), leftChild(nullptr), rightChild(nullptr) {}
};

struct Tree {
    TreeNode * root;
    Tree() : root(nullptr) {}
    void destroy(TreeNode * n) {
        if (!n) { return; }
        destroy(n->leftChild);
        destroy(n->rightChild);
        delete n;
    }
    ~Tree() { destroy(root); }
    void insertRandomlyAux(TreeNode * parent, TreeNode * n) {
        assert(n && parent);
        if (rand() % 2) {
            if (parent->leftChild) {
                insertRandomlyAux(parent->leftChild, n);
            } else {
                parent->leftChild = n;
            }
        } else {
            if (parent->rightChild) {
                insertRandomlyAux(parent->rightChild, n);
            } else {
                parent->rightChild = n;
            }
        }
    }
    void insertRandomly(TreeNode * n) {
        if (!n) { return; }
        if (!root) {
            root = n;
            return;
        }
        insertRandomlyAux(root, n);
    }
    void fill(size_t n) {
        for (size_t i = 0; i < n; ++i) {
            insertRandomly(new TreeNode(rand() % 10));
        }
    }
    void print(std::ostream & os, TreeNode * n, int indent) const {
        if (!n) { return; }
        print(os, n->rightChild, indent + 5);
        os << std::setw(indent) << n->val << std::endl;
        print(os, n->leftChild, indent + 5);
    }
    void flipAux(TreeNode * n) {
        assert(n);
        if (n->rightChild && n->leftChild) {
            swap(n->rightChild->val, n->leftChild->val);
            flipAux(n->rightChild);
            flipAux(n->leftChild);
        } else if (n->rightChild) {
            n->leftChild = n->rightChild;
            n->rightChild = nullptr;
            flipAux(n->leftChild);
        } else if (n->leftChild) {
            n->rightChild = n->leftChild;
            n->leftChild = nullptr;
            flipAux(n->rightChild);
        }
    }
    void flip() {
        if (!root) { return; }
        flipAux(root);
    }
};

std::ostream & operator<<(std::ostream & os, const Tree & t) {
    t.print(os, t.root, 0);
    return os;
}

int main() {
    srand(time(0));
    Tree t;
    t.fill(10);
    std::cout << t << std::endl;
    std::cout << "Flipped:\n";
    t.flip();
    std::cout << t << std::endl;
    return 0;
}
