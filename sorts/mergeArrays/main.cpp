#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <assert.h>

#define MAX_SIZE 10
#define NUM_ITERATIONS 1000000

std::ostream & operator<<(std::ostream & os, const std::vector<int> v) {
    for (auto it = v.begin(); it != v.end(); ++it) {
        os << std::setw(3) << *it;
    }
    return os;
}

void mergeInto(std::vector<int> & B, std::vector<int> & A) {
    auto oldBCapacity = B.capacity(); // Ensure A and B stay fixed-size
    auto oldACapacity = A.capacity();
    // In order to treat A like an array of its capacity, fill it with values
    int actualASize = A.size();
    while (A.size() != A.capacity()) {
        A.push_back(-1);
    }
    // Merge
    int aIndex = actualASize - 1;
    int bIndex = B.size() - 1;
    int writeIndex = A.capacity() - 1;
    while (aIndex >= 0 && bIndex >= 0) {
        if (A[aIndex] >= B[bIndex]) {
            A[writeIndex--] = A[aIndex--];
        } else {
            A[writeIndex--] = B[bIndex--];
        }
    }
    // One of A or B has runned out
    if (aIndex >= 0) {
        while (writeIndex >= 0) {
            A[writeIndex--] = A[aIndex--];
        }
    }
    if (bIndex >= 0) {
        while (writeIndex >= 0) {
            A[writeIndex--] = B[bIndex--];
        }
    }
    assert(B.capacity() == oldBCapacity);
    assert(A.capacity() == oldACapacity);
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        std::vector<int> A;
        A.reserve(rand() % MAX_SIZE + 1); // Set random capacity
        size_t aSize = rand() % A.capacity() + 1;
        for (int i = 0; i < aSize; ++i) {
            A.push_back(rand() % 100);
        }
        std::sort(A.begin(), A.end());
        std::vector<int> B;
        B.reserve(A.capacity() - aSize);
        for (int i = 0; i < B.capacity(); ++i) {
            B.push_back(rand() % 100);
        }
        std::sort(B.begin(), B.end());
        /* For debugging
        std::cout << "B: " << B << " (" << B.size() << "/" << B.capacity()
                  << ")" << std::endl;
        std::cout << "A: " << A << " (" << A.size() << "/" << A.capacity()
                  << ")" << std::endl;
        std::cout << "Merging B into A\n";
        */
        mergeInto(B, A);
        // std::cout << "A: " << A << std::endl;
        // Test by asserting against std::algorithm's merge if it exists
        assert(std::is_sorted(A.begin(), A.end()));
        assert(A.size() == aSize + B.capacity());
    }
    return 0;
}
