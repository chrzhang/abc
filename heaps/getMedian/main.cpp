#include <iostream>
#include <vector>
#include <algorithm>
#include <cassert>

#define N 1000

// Get the median of a set of data (maintain the median as data changes)

enum HeapType { MIN, MAX };

bool compareLess(const int & i1, const int & i2) {
    return i1 < i2;
}

bool compareMore(const int & i1, const int & i2) {
    return i1 > i2;
}

struct Heap {
    std::vector<int> v;
    HeapType t;
    Heap(HeapType t) : t(t) {}
    size_t size() const {
        return v.size();
    }
    bool empty() const {
        return v.empty();
    }
};

void addToHeap(Heap & h, int i) {
    h.v.push_back(i);
    switch (h.t) {
        case MIN: {
            std::push_heap(h.v.begin(), h.v.end(), compareMore);
            break;
        }
        case MAX: {
            std::push_heap(h.v.begin(), h.v.end(), compareLess);
            break;
        }
    }
}

// Removes from the smaller heap and adds to bigger heap
void balanceStep(Heap & minHeap, Heap & maxHeap) {
    assert(minHeap.size() != maxHeap.size());
    Heap & biggerHeap = minHeap.size() > maxHeap.size() ? minHeap :
                                                          maxHeap;
    Heap & smallerHeap = minHeap.size() > maxHeap.size() ? maxHeap :
                                                           minHeap;
    switch(biggerHeap.t) {
        case MIN: {
            std::pop_heap(biggerHeap.v.begin(), biggerHeap.v.end(),
                          compareMore);
            break;
        }
        case MAX: {
            std::pop_heap(biggerHeap.v.begin(), biggerHeap.v.end(),
                          compareLess);
            break;
        }
    }
    int i = biggerHeap.v.back();
    biggerHeap.v.pop_back();
    addToHeap(smallerHeap, i);
}

double mean(int a, int b) {
    return (((double) a) + b) / 2;
}

size_t absDiff(size_t a, size_t b) {
    return a > b ? a - b : b - a;
}

// Balances (makes heaps same size or as close as possible) and finds median
// by examining roots of each heap
double getMedian(Heap & minHeap, Heap & maxHeap) {
    auto totalSize = minHeap.size() + maxHeap.size();
    if (totalSize == 0) {
        std::cout << "No elements. Cannot find median\n";
        assert(false);
    } else if (totalSize == 1) {
        return minHeap.empty() ? *maxHeap.v.begin() : *minHeap.v.begin();
    }
    if (totalSize % 2 == 0) { // Even amount of elements
        while (minHeap.size() != totalSize / 2 &&
               maxHeap.size() != totalSize / 2) {
            balanceStep(minHeap, maxHeap);
        }
        return mean(*minHeap.v.begin(), *maxHeap.v.begin());
    } else { // Odd number of elements
        while (1 != absDiff(minHeap.size(), maxHeap.size())) {
            balanceStep(minHeap, maxHeap);
        }
        double minBegin = *minHeap.v.begin();
        double maxBegin = *maxHeap.v.begin();
        return minHeap.size() > maxHeap.size() ? minBegin : maxBegin;
    }
}

int main() {
    // Use 2 heaps, one is a min, the other a max
    Heap minHeap(MIN);
    Heap maxHeap(MAX);
    std::vector<int> baseline; // For testing
    for (int i = 0, j = rand() % 100; i < N; ++i, j = rand() % 100) {
        // When adding a new element, pick appropriate heap
        Heap * heapToAddTo = &minHeap; // Default to min-heap
        if (!maxHeap.empty() && j <= maxHeap.v.front()) {
            heapToAddTo = &maxHeap;
        }
        addToHeap(*heapToAddTo, j);
        baseline.push_back(j);
        auto median = getMedian(minHeap, maxHeap);
        std::sort(baseline.begin(), baseline.end());
        if (baseline.size() == 1) {
            assert(median == baseline.front());
        } else if (baseline.size() % 2 == 0) {
            assert(median == mean(baseline[baseline.size() / 2 - 1],
                                  baseline[baseline.size() / 2]));
        } else if (baseline.size() % 2 == 1) {
            assert(median == baseline[baseline.size() / 2]);
        }
    }
}
