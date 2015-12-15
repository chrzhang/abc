#include <iostream>
#include <iomanip>
#include <assert.h>
#include <cstdlib>
#include <ctime>
#include <queue>
#include <vector>

#define NUM_NODES 30

struct TreeNode {
    int val;
    TreeNode * leftChild;
    TreeNode * rightChild;
    TreeNode(int val) {
        this->val = val;
        leftChild = rightChild = nullptr;
    }
};

struct Tree {
    TreeNode * root;
    size_t size;
    void insertRandomly(TreeNode * parent, TreeNode * n) {
        assert(parent && n);
        switch (rand() % 2) {
            case 0: { // Left
                if (!parent->leftChild) {
                    parent->leftChild = n;
                } else {
                    insertRandomly(parent->leftChild, n);
                }
                break;
            }
            case 1: { // Right
                if (!parent->rightChild) {
                    parent->rightChild = n;
                } else {
                    insertRandomly(parent->rightChild, n);
                }
                break;
            }
        }
    }
    Tree(int numNodes) { // Fills the tree randomly
        if (0 == numNodes) { return; }
        root = new TreeNode(rand() % 100);
        for (int i = 1; i < numNodes; ++i) {
            TreeNode * n = new TreeNode(rand() % 100);
            insertRandomly(root, n);
        }
        size = numNodes;
    }
    void destroyHelper(TreeNode * parent) {
        if (!parent) { return; }
        destroyHelper(parent->leftChild);
        destroyHelper(parent->rightChild);
        delete parent;
    }
    ~Tree() {
        destroyHelper(root);
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
    std::vector<std::vector<TreeNode *>> getNodesAtEachDepth() const {
        std::vector<std::vector<TreeNode *>> allDepthsList;
        std::vector<TreeNode *> currDepthList;
        std::queue<TreeNode *> q;
        // Enqueue the root
        q.push(root);
        int numNodesLeftForCurrDepth = 1;
        int numNodesLeftForNextDepth = 0;
        while (!q.empty()) {
            TreeNode * popped = q.front();
            currDepthList.push_back(popped);
            numNodesLeftForCurrDepth -= 1;
            if (popped->leftChild) {
                q.push(popped->leftChild);
                ++numNodesLeftForNextDepth;
            }
            if (popped->rightChild) {
                q.push(popped->rightChild);
                ++numNodesLeftForNextDepth;
            }
            if (numNodesLeftForCurrDepth == 0) {
                numNodesLeftForCurrDepth = numNodesLeftForNextDepth;
                numNodesLeftForNextDepth = 0;
                allDepthsList.push_back(std::vector<TreeNode *>(currDepthList));
                currDepthList.clear();
            }
            q.pop();
        }
        return allDepthsList;
    }
};

std::ostream & operator<<(std::ostream & os, const Tree & t) {
    t.print(os);
    return os;
}

int main() {
    srand(time(0));
    Tree t(NUM_NODES);
    std::cout << t << std::endl;
    auto listOfLists = t.getNodesAtEachDepth();
    for (auto it1 = listOfLists.begin(); it1 != listOfLists.end(); ++it1) {
        std::cout << "Depth: " << 1 + it1 - listOfLists.begin()
                  << " contains: ";
        for (auto it2 = it1->begin(); it2 != it1->end(); ++it2) {
            std::cout << (*it2)->val << " ";
        }
        std::cout << std::endl;
    }
    return 0;
}
