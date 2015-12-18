#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <ctime>
#include <assert.h>
#include <unordered_map>
#include <list>

#define NUM_NODES 10

// Given two nodes in a binary tree, find their lowest common ancestor

struct TreeNode {
    TreeNode * leftChild, * rightChild;
    int val;
    TreeNode(int val) {
        this->val = val;
        leftChild = rightChild = nullptr;
    }
};

struct Tree {
    TreeNode * root;
    std::unordered_map<int, TreeNode *> valToAddr; // For testing
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
    void insertRandomlyAux(TreeNode * parent, TreeNode * n) {
        assert(parent && n);
        switch(rand() % 2) {
            case 0: {
                if (!parent->leftChild) {
                    parent->leftChild = n;
                } else {
                    insertRandomlyAux(parent->leftChild, n);
                }
                break;
            }
            case 1: {
                if (!parent->rightChild) {
                    parent->rightChild = n;
                } else {
                    insertRandomlyAux(parent->rightChild, n);
                }
                break;
            }
        }
    }
    void insertRandomly(TreeNode * n) {
        if (!n) { return; }
        valToAddr[n->val] = n;
        if (!root) {
            root = n;
        } else {
            insertRandomlyAux(root, n);
        }
    }
    bool contains(TreeNode * parent, TreeNode * n) const {
        if (!parent || !n) { return false; }
        if (parent == n) { return true; }
        return (contains(parent->leftChild, n) ||
                contains(parent->rightChild, n));

    }
    bool findPathToNodeAux(std::list<bool> & leftRightDecisions,
                           TreeNode * parent, TreeNode * n) const {
        if (!parent || !n) { return false; }
        if (parent == n) { return true; }
        bool leftHasN = findPathToNodeAux(leftRightDecisions, parent->leftChild,
                                          n);
        if (leftHasN) {
            leftRightDecisions.push_front(0);
            return true;
        }
        bool rightHasN = findPathToNodeAux(leftRightDecisions,
                                           parent->rightChild, n);
        if (rightHasN) {
            leftRightDecisions.push_front(1);
            return true;
        }
        return false;
    }
    std::list<bool> findPathToNode(TreeNode *n) const {
        std::list<bool> leftRightDecisions;
        findPathToNodeAux(leftRightDecisions, root, n);
        return leftRightDecisions;
    }
    TreeNode * findLCAStorage(TreeNode * n1, TreeNode * n2) const {
        std::list<bool> lod1 = findPathToNode(n1);
        std::list<bool> lod2 = findPathToNode(n2);
        TreeNode * currLCA = root;
        for (auto it1 = lod1.begin(), it2 = lod2.begin();
             it1 != lod1.end() && it2 != lod2.end(); ++it1, ++it2) {
            if (*it1 != *it2) {
                break;
            } else {
                currLCA = *it1 ? currLCA->rightChild : currLCA->leftChild;
            }
        }
        if (currLCA) {
            return currLCA;
        } else {
            return n1;
        }
    }
    TreeNode * findLCAAux(TreeNode * parent,
                          TreeNode * n1,
                          TreeNode * n2) const {
        if (!parent) { return nullptr; }
        if (n1 == parent || n2 == parent) { return parent; }
        bool leftHasN1 = contains(parent->leftChild, n1);
        bool rightHasN2 = contains(parent->rightChild, n2);
        if (leftHasN1 && rightHasN2) {
            return parent;
        }
        bool leftHasN2 = contains(parent->leftChild, n2);
        bool rightHasN1 = contains(parent->rightChild, n1);
        if (leftHasN2 && rightHasN1) {
            return parent;
        }
        if (leftHasN1 && leftHasN2) {
            return findLCAAux(parent->leftChild, n1, n2);
        }
        if (rightHasN2 && rightHasN1) {
            return findLCAAux(parent->rightChild, n1, n2);
        }
        return nullptr;
    }
    TreeNode * findLCA(TreeNode * n1, TreeNode * n2) const {
        return findLCAAux(root, n1, n2);
    }
    // Return n1 if n1 is in the subtree but not n2 (same for n2)
    // A returned node that is neither n1 or n2 is a valid common ancestor
    TreeNode * findLCAOptimizedAux(TreeNode * parent,
                                   TreeNode * n1,
                                   TreeNode * n2) const {
        if (!parent) { return nullptr; }
        if (n1 == parent && n2 == parent) { return parent; }
        TreeNode * t1 = findLCAOptimizedAux(parent->leftChild, n1, n2);
        if (t1 && t1 != n1 && t1 != n2) { return t1; } // Found already
        TreeNode * t2 = findLCAOptimizedAux(parent->rightChild, n1, n2);
        if (t2 && t2 != n1 && t2 != n2) { return t2; } // Found already
        if ((t1 && t2) || (parent == n1 || parent == n2)) {
            return parent; // First encounter of common ancestor
        }
        return t1 ? t1 : t2;
    }
    TreeNode * findLCAOptimized(TreeNode * n1, TreeNode * n2) const {
        return findLCAOptimizedAux(root, n1, n2);
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
    Tree t;
    for (int i = 0; i < NUM_NODES; ++i) {
        t.insertRandomly(new TreeNode(i));
    }
    std::cout << t << std::endl;
    for (auto it1 = t.valToAddr.begin(); it1 != t.valToAddr.end(); ++it1) {
        for (auto it2 = t.valToAddr.begin(); it2 != t.valToAddr.end(); ++it2) {
            TreeNode * t1 = t.findLCA(it1->second, it2->second);
            TreeNode * t2 = t.findLCAStorage(it1->second, it2->second);
            assert(t1 == t2);
            TreeNode * t3 = t.findLCAOptimized(it1->second, it2->second);
            assert(t1 == t3);
            std::cout << "LCA of " << it1->first << " and " << it2->first
                      << " is " << t1->val << "\t";
        }
    }
    std::cout << std::endl;
    return 0;
}
