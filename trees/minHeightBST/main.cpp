#include <iostream>
#include <vector>
#include <assert.h>
#include <iomanip>
#include <cmath>

#define NUM_NODES 1000
#define NUM_ITERATIONS 1000

// Given a sorted array, generate a binary search tree of minimum height

struct Node {
    int val;
    Node * leftChild, * rightChild;
    Node(int val) {
        this->val = val;
        leftChild = rightChild = nullptr;
    }
};

int max(int a, int b) {
    return (a > b) ? a : b;
}

struct BinarySearchTree {
    Node * root;
    size_t size;
    BinarySearchTree() {
        size = 0;
        root = nullptr;
    }
    void destroyHelper(Node * n) {
        if (!n) { return; }
        destroyHelper(n->leftChild);
        destroyHelper(n->rightChild);
        delete n;
    }
    ~BinarySearchTree() {
        destroyHelper(root);
    }
    void insertHelper(Node * parent, Node * n) {
        assert(parent && n);
        if (n->val < parent->val) {
            if (parent->leftChild) {
                insertHelper(parent->leftChild, n);
            } else {
                parent->leftChild = n;
            }
        } else {
            if (parent->rightChild) {
                insertHelper(parent->rightChild, n);
            } else {
                parent->rightChild = n;
            }
        }
    }
    void insert(Node * n) {
        if (!n) { return; }
        ++size;
        n->leftChild = n->rightChild = nullptr;
        if (!root) {
            root = n;
        } else {
            insertHelper(root, n);
        }
    }
    void fromSortedArrayAux(const std::vector<int> & arr, int startIndex,
                            int endIndex) {
        if (startIndex > endIndex) {
            return;
        }
        int midpt = startIndex + (endIndex - startIndex) / 2;
        insert(new Node(arr[midpt]));
        fromSortedArrayAux(arr, startIndex, midpt - 1);
        fromSortedArrayAux(arr, midpt + 1, endIndex);
    }
    void fromSortedArray(const std::vector<int> & arr) {
        fromSortedArrayAux(arr, 0, arr.size() - 1);
    }
    Node * fromSortedArrayOptAux(Node * parent, const std::vector<int> & arr,
                                 int startIndex, int endIndex) {
        if (startIndex > endIndex || !parent) {
            return nullptr;
        }
        int midpt = startIndex + (endIndex - startIndex) / 2;
        Node * n = new Node(arr[midpt]);
        ++size;
        n->leftChild = fromSortedArrayOptAux(n, arr, startIndex, midpt - 1);
        n->rightChild = fromSortedArrayOptAux(n, arr, midpt + 1, endIndex);
        return n;
    }
    void fromSortedArrayOpt(const std::vector<int> & arr) {
        if (arr.empty()) return;
        int midpt = (arr.size() - 1) / 2;
        root = new Node(arr[midpt]);
        ++size;
        root->leftChild = fromSortedArrayOptAux(root, arr, 0, midpt - 1);
        root->rightChild = fromSortedArrayOptAux(root, arr, midpt + 1,
                                                 arr.size() - 1);
    }
    int getHeightAux(Node * n) const {
        if (!n) { return 0; }
        return 1 + max(getHeightAux(n->leftChild), getHeightAux(n->rightChild));
    }
    int getHeight() const {
        return getHeightAux(root);
    }
};

void printHelper(std::ostream & os, int indent, Node * n) {
    if (n) {
        printHelper(os, indent + 5, n->rightChild);
        os << std::setw(indent) << " " << n->val << std::endl;
        printHelper(os, indent + 5, n->leftChild);
    }
}

std::ostream & operator<<(std::ostream & os, const BinarySearchTree & bst) {
    printHelper(os, 0, bst.root);
    return os;
}

int heightNeededForNodes(int numNodes) {
    int nodesLeft = numNodes;
    int currHeight = 1;
    while (nodesLeft > 0) {
        nodesLeft -= pow(2, currHeight - 1);
        ++currHeight;
    }
    return currHeight - 1;
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        std::vector<int> sortedArr;
        int randNumNodes = rand() % NUM_NODES;
        for (int i = 0; i < randNumNodes; ++i) {
            sortedArr.push_back(i);
        }
        {
            BinarySearchTree b;
            b.fromSortedArrayOpt(sortedArr);
            assert(b.getHeight() == heightNeededForNodes(b.size));
        }
        {
            BinarySearchTree b;
            b.fromSortedArray(sortedArr);
            assert(b.getHeight() == heightNeededForNodes(b.size));
        }
    }
    return 0;
}
