#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <ctime>
#include <cassert>
#include <vector>

// Find the smallest million in a billion numbers

void swap(int & a, int & b) {
    auto temp = a;
    a = b;
    b = temp;
}

struct OrderStatisticHeap {
    std::vector<int> v;
    size_t numberOfSmallestItems;
    OrderStatisticHeap(size_t k) {
        numberOfSmallestItems = k;
    }
    int getLeftChildOf(int i) const {
        if (i < 0 || i >= v.size()) { return -1; }
        auto lc = 2 * i + 1;
        if (lc >= v.size()) { return -1; }
        return lc;
    }
    int getRightChildOf(int i) const {
        if (i < 0 || i >= v.size()) { return -1; }
        auto rc = 2 * i + 2;
        if (rc >= v.size()) { return -1; }
        return rc;
    }
    int getParentOf(int i) const {
        if (i <= 0 || i >= v.size()) { return -1; }
        return (i - 1) / 2;
    }
    void addToHeap(int i) {
        assert(v.size() < numberOfSmallestItems);
        // Add it to the end and percolate up
        v.push_back(i);
        int currIndex = v.size() - 1;
        while (currIndex > 0) {
            int parentIndex = getParentOf(currIndex);
            if (parentIndex == -1) { break; }
            if (v[currIndex] > v[parentIndex]) {
                swap(v[currIndex], v[parentIndex]);
                currIndex = parentIndex;
            } else {
                break;
            }
        }
    }
    void feedDatum(int i) {
        if (v.size() < numberOfSmallestItems) {
            // Add it to the heap normally
            addToHeap(i);
        } else {
            // Consider the current heap min and if the number is smaller...
            if (i < v[0]) {
                v[0] = i;
                int currentIndex = 0;
                while (currentIndex < v.size()) {
                    // Percolate down to maintain heap-ness
                    auto lci = getLeftChildOf(currentIndex);
                    auto rci = getRightChildOf(currentIndex);
                    int biggerChild = -1;
                    if (lci > 0 && lci < v.size() &&
                        rci > 0 && rci < v.size()) {
                        biggerChild = v[lci] > v[rci] ? lci : rci;
                    } else if (lci > 0 && lci < v.size()) {
                        biggerChild = lci;
                    } else if (rci > 0 && rci < v.size()) {
                        biggerChild = rci;
                    } else {
                        break;
                    }
                    // Both children exist
                    bool needToPercolate = v[currentIndex] < v[biggerChild];
                    if (needToPercolate) {
                        swap(v[currentIndex], v[biggerChild]);
                        currentIndex = biggerChild;
                    } else {
                        break;
                    }
                }
            }
        }
    }
    void print(std::ostream & os, int i, int indent) const {
        if (i < 0 || i >= v.size()) { return; }
        print(os, getRightChildOf(i), indent + 5);
        os << std::setw(indent) << v[i] << std::endl;
        print(os, getLeftChildOf(i), indent + 5);
    }
};

std::ostream & operator<<(std::ostream & os, const OrderStatisticHeap & h) {
    h.print(os, 0, 0);
    return os;
}

int main() {
    srand(time(0));
    { // Small example for testing
        const size_t k = 10; // Smallest k items
        std::vector<int> vTest; // To test against
        OrderStatisticHeap h(k);
        for (int i = 0; i < 1000; ++i) {
            auto r = rand() % 1000;
            std::cout << i << "\r";
            h.feedDatum(r);
            vTest.push_back(r);
        }
        std::sort(vTest.begin(), vTest.end());
        auto smallestKItems = std::vector<int>(vTest.begin(),
                                               vTest.begin() + k);
        auto itemsFromHeap = h.v; // Copy from heap
        std::sort(itemsFromHeap.begin(), itemsFromHeap.end());
        assert(smallestKItems == itemsFromHeap);
    }
    const size_t k = 1000000;
    OrderStatisticHeap h(k);
    for (long long i = 0; i < 1000000000; ++i) { // Feed a billion numbers
        std::cout << i << " numbers read...\r";
        h.feedDatum(rand() % 100);
    }
    for (auto it = h.v.begin(); it != h.v.end(); ++it) {
        std::cout << *it << " ";
    }
    std::cout << std::endl;
    return 0;
}
