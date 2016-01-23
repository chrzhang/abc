#include <iostream>
#include <iomanip>
#include <vector>
#include <cassert>
#include <ctime>
#include <cassert>

#define NUM_ITERATIONS 1000000

// Implement a basic heap

void swap(int & a, int & b) {
    auto temp = a;
    a = b;
    b = temp;
}

bool fallsInBounds(int index, const std::vector<int> & v) {
    if (index < 0) { return false; }
    if (index >= v.size()) { return false; }
    return true;
}

enum HeapType { MIN, MAX };

struct Comparator {
    bool operator()(HeapType ht, int x, int y) const {
        if (ht == MIN) {
            return x <= y;
        } else if (ht == MAX) {
            return x >= y;
        } else {
            assert(false); // New type of heap?
            return false;
        }
    }
};

struct Heap {
    std::vector<int> nodes;
    Comparator cmp;
    HeapType heapType;
    Heap(HeapType ht) : heapType(ht) {}
    size_t getSize() const {
        return nodes.size();
    }
    bool empty() const {
        return nodes.empty();
    }
    bool compare(int a, int b) const {
        return cmp(heapType, a, b);
    }
    int getIndexOfLeftChild(int indexOfParentNode) const {
        if (!fallsInBounds(indexOfParentNode, nodes)) { return -1; }
        auto lci = 2 * indexOfParentNode + 1;
        if (!fallsInBounds(lci, nodes)) { return -1; }
        return lci;
    }
    int getIndexOfRightChild(int indexOfParentNode) const {
        if (!fallsInBounds(indexOfParentNode, nodes)) { return -1; }
        auto rci = 2 * indexOfParentNode + 2;
        if (!fallsInBounds(rci, nodes)) { return -1; }
        return rci;
    }
    int getIndexOfParent(int indexOfChildNode) const {
        if (!fallsInBounds(indexOfChildNode, nodes)) { return -1; }
        if (indexOfChildNode == 0) { return -1; }
        return (indexOfChildNode - 1) / 2;
    }
    bool isHeapAux(int currNodeIndex) const {
        auto lci = getIndexOfLeftChild(currNodeIndex);
        auto rci = getIndexOfRightChild(currNodeIndex);
        if (fallsInBounds(lci, nodes) && !compare(nodes[currNodeIndex],
                                                  nodes[lci])) {
            return false;
        }
        if (fallsInBounds(rci, nodes) && !compare(nodes[currNodeIndex],
                                                  nodes[rci])) {
            return false;
        }
        bool leftIsHeap = true;
        if (fallsInBounds(lci, nodes)) {
            leftIsHeap = isHeapAux(lci);
        }
        bool rightIsHeap = true;
        if (fallsInBounds(rci, nodes)) {
            rightIsHeap = isHeapAux(rci);
        }
        return leftIsHeap && rightIsHeap;
    }
    bool isHeap() const {
        if (empty()) { return true; }
        return isHeapAux(0);
    }
    void print(std::ostream & os, int currNodeIndex, int indent) const {
        if (!fallsInBounds(currNodeIndex, nodes)) { return; }
        print(os, getIndexOfRightChild(currNodeIndex), indent + 5);
        os << std::setw(indent) << nodes[currNodeIndex] << std::endl;
        print(os, getIndexOfLeftChild(currNodeIndex), indent + 5);
    }
    void add(int val) {
        // Put new value to the end of the heap (last level, rightmost spot)
        // and percolate upwards
        nodes.push_back(val);
        int currIndex = nodes.size() - 1;
        assert(val == nodes[currIndex]);
        while (fallsInBounds(currIndex, nodes)) {
            int parentIndex = getIndexOfParent(currIndex);
            if (!fallsInBounds(parentIndex, nodes)) { return; }
            if (!compare(nodes[parentIndex], nodes[currIndex])) {
                // Keep percolating up
                swap(nodes[parentIndex], nodes[currIndex]);
                currIndex = parentIndex;
            } else {
                break;
            }
        }
        assert(isHeap());
    }
    // Remove the root, replace it with the last value and percolate down
    int popTop() { // Undefined when empty
        if (empty()) {
            std::cout << "Cannot getMin/Max from empty heap\n";
            assert(false);
        }
        auto top = nodes[0];
        swap(nodes[0], nodes[nodes.size() - 1]);
        nodes.pop_back();
        int currIndex = 0;
        while (fallsInBounds(currIndex, nodes)) {
            int lci = getIndexOfLeftChild(currIndex);
            int rci = getIndexOfRightChild(currIndex);
            bool satisfiesHeapProperty = true; // Whether to percolate down
            if (fallsInBounds(lci, nodes)) {
                satisfiesHeapProperty = compare(nodes[currIndex], nodes[lci]);
            }
            if (fallsInBounds(rci, nodes)) {
                satisfiesHeapProperty = satisfiesHeapProperty &&
                                        compare(nodes[currIndex], nodes[rci]);
            }
            if (satisfiesHeapProperty) { break; }
            // Swap with bigger/lesser child (depending on heap type)
            int biggerChildIndex = -1;
            if (fallsInBounds(lci, nodes) && fallsInBounds(rci, nodes)) {
                biggerChildIndex =
                    compare(nodes[lci], nodes[rci]) ? lci : rci;
            } else if (fallsInBounds(lci, nodes)) {
                biggerChildIndex = lci;
            } else if (fallsInBounds(rci, nodes)) {
                biggerChildIndex = rci;
            } else {
                break;
            }
            assert(fallsInBounds(biggerChildIndex, nodes));
            swap(nodes[currIndex], nodes[biggerChildIndex]);
            currIndex = biggerChildIndex;
        }
        assert(isHeap());
        return top;
    }
};

std::ostream & operator<<(std::ostream & os, const Heap & h) {
    h.print(os, 0, 0);
    return os;
}

std::ostream & operator<<(std::ostream & os, const std::vector<int> & v) {
    for (auto it = v.begin(); it != v.end(); ++it) {
        os << *it << " ";
    }
    os << std::endl;
    return os;
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        std::cout << "Testing: "
                  << 100 * ((double) iteration) / NUM_ITERATIONS << "%\r";
        // Test adding values
        Heap h(MIN);
        for (int i = 0; i < rand() % 100; ++i) {
            auto r = rand() % 100;
            h.add(r);
        }
        // Test removing values
        std::vector<int> heapSorted;
        while (!h.empty()) {
            heapSorted.push_back(h.popTop());
        }
        assert(std::is_sorted(heapSorted.begin(), heapSorted.end()));
    }
    return 0;
}
