#include <iostream>
#include <iomanip>
#include <assert.h>
#include <cstdlib>
#include <ctime>
#include <vector>

#define NUM_NODES 10

// Find all paths in a tree that sum to a given number

struct TreeNode {
    int val, id;
    static int counter;
    TreeNode * leftChild, * rightChild;
    TreeNode(int val) {
        this->val = val;
        leftChild = rightChild = nullptr;
        id = ++counter;
    }
};

int TreeNode::counter = 0;

struct Tree {
    TreeNode * root;
    Tree() {
        root = nullptr;
        for (int i = 0; i < NUM_NODES; ++i) {
            insertRandomly(new TreeNode(rand() % 10));
        }
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
    void insertRandomlyHelper(TreeNode * parent, TreeNode * n) {
        assert(parent && n);
        if (rand() % 2) {
            if (parent->leftChild) {
                insertRandomlyHelper(parent->leftChild, n);
            } else {
                parent->leftChild = n;
            }
        } else {
            if (parent->rightChild) {
                insertRandomlyHelper(parent->rightChild, n);
            } else {
                parent->rightChild = n;
            }
        }
    }
    void insertRandomly(TreeNode * n) {
        if (!root) {
            root = n;
        } else {
            insertRandomlyHelper(root, n);
        }
    }
    void printAux(std::ostream & os, int indent, TreeNode * n) const {
        if (!n) { return; }
        printAux(os, indent + 10, n->rightChild);
        os << std::setw(indent) << n->val << "(" << n->id << ")" << std::endl;
        printAux(os, indent + 10, n->leftChild);
    }
    void print(std::ostream & os) const {
        printAux(os, 0, root);
    }
    void findAllPathsFromRootAux(std::vector<std::vector<TreeNode *>> & allPaths,
                                 std::vector<TreeNode *> currPath,
                                 TreeNode * parent, int sumSoFar, int target) {
        sumSoFar += parent->val;
        currPath.push_back(parent);
        if (sumSoFar > target) {
            return;
        }
        if (sumSoFar == target) {
            allPaths.push_back(currPath);
        }
        if (parent->leftChild) {
            findAllPathsFromRootAux(allPaths, currPath, parent->leftChild,
                                    sumSoFar, target);
        }
        if (parent->rightChild) {
            findAllPathsFromRootAux(allPaths, currPath, parent->rightChild,
                                    sumSoFar, target);
        }
    }
    void findAllPathsFromRoot(std::vector<std::vector<TreeNode *>> & allPaths,
                              TreeNode * root, int sum) {
        std::vector<TreeNode *> currPath;
        findAllPathsFromRootAux(allPaths, currPath, root, 0, sum);
    }
    // Traversal where each node acts as a root and potential paths are checked
    void checkEachNodeAux(std::vector<std::vector<TreeNode *>> & allPaths,
                          TreeNode * n, int sum) {
        if (!n) { return; }
        findAllPathsFromRoot(allPaths, n, sum);
        checkEachNodeAux(allPaths, n->leftChild, sum);
        checkEachNodeAux(allPaths, n->rightChild, sum);
    }
    void checkEachNodeForPathsToSum(int sum) {
        std::vector<std::vector<TreeNode *>> allPaths;
        checkEachNodeAux(allPaths, root, sum);
        if (!allPaths.empty()) {
            std::cout << "Paths that sum to " << sum << std::endl;
        }
        for (auto eachPathIt = allPaths.begin(); eachPathIt != allPaths.end();
             ++eachPathIt) {
            std::cout << "Path:\t";
            for (auto it = eachPathIt->begin(); it != eachPathIt->end(); ++it) {
                std::cout << (*it)->val << "(" << (*it)->id << ") ";
            }
            std::cout << std::endl;
        }
    }
};

std::ostream & operator<<(std::ostream & os, const Tree & t) {
    t.print(os);
    return os;
}

int main() {
    srand(time(0));
    std::cout << "The horizontal tree values' unique IDs are in parentheses.\n";
    Tree t;
    std::cout << t << std::endl;
    for (int sum = 0; sum < 9 * NUM_NODES; ++sum) {
        t.checkEachNodeForPathsToSum(sum);
    }
    return 0;
}
