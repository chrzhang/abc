#include <iostream>
#include <iomanip>
#include <assert.h>
#include <cstdlib>
#include <ctime>

#define N 15
#define NUM_ITERATIONS 100000

// Find indices of smallest unsorted subarray

struct Indices {
    int beginIndex, endIndex;
    Indices(int b, int e) : beginIndex(b), endIndex(e) {}
};

void printArray(int arr[N]) {
    for (int i = 0; i < N; ++i) {
        std::cout << "[" << i << "]: " << std::setw(2) << arr[i] << " ";
    }
    std::cout << std::endl;
}

void swap(int & a, int & b) {
    auto temp = a;
    a = b;
    b = temp;
}

bool onLeftOf(int i, int p) {
    return i < p;
}

bool onRightOf(int i, int p) {
    return i > p;
}

// Return index where partition finally ends up
int partition(int pivotIndex, int beginIndex, int endIndex, int arr[N]) {
    for (int i = beginIndex; i <= endIndex; ++i) {
        if (onRightOf(i, pivotIndex)) {
            if (arr[i] < arr[pivotIndex]) {
                auto temp = arr[i];
                for (int j = i; j > beginIndex; --j) {
                    arr[j] = arr[j - 1];
                }
                arr[beginIndex] = temp;
                ++pivotIndex;
            }
        } else if (onLeftOf(i, pivotIndex)) {
            if (arr[i] > arr[pivotIndex]) {
                swap(arr[i], arr[pivotIndex]);
                pivotIndex = i;
            }
        }
    }
    return pivotIndex;
}

void qsort(int beginIndex, int endIndex, int arr[N]) {
    if (beginIndex >= endIndex) { return; }
    int pivotIndex = beginIndex + rand() % (endIndex - beginIndex + 1);
    auto finalPivotIndex = partition(pivotIndex, beginIndex, endIndex, arr);
    qsort(beginIndex, finalPivotIndex - 1, arr);
    qsort(finalPivotIndex + 1, endIndex, arr);
}

void shuffle(int arr[N]) {
    for (int i = 0; i < N - 1; ++i) {
        swap(arr[i], arr[rand() % (N - i) + i]);
    }
}

bool isSorted(int arr[N]) {
    for (int i = 1; i < N; ++i) {
        if (arr[i] < arr[i - 1]) { return false; }
    }
    return true;
}

Indices getSubarrayIndices(int arr[N]) {
    // Store a left to right running maximum for subarrays ending at each index
    int maxFromLeft[N];
    maxFromLeft[0] = arr[0];
    for (int i = 1; i < N; ++i) {
        if (arr[i] > maxFromLeft[i - 1]) {
            maxFromLeft[i] = arr[i];
        } else {
            maxFromLeft[i] = maxFromLeft[i - 1];
        }
    }
    // Store a right to left running minimum for subarrays ending at each index
    int minFromRight[N];
    minFromRight[N - 1] = arr[N - 1];
    for (int i = N - 2; i >= 0; --i) {
        if (arr[i] < minFromRight[i + 1]) {
            minFromRight[i] = arr[i];
        } else {
            minFromRight[i] = minFromRight[i + 1];
        }
    }
    // When the values for the running min/max are the same, the corr. subarrays
    // are sorted. The point at which they differ denotes the beginning and end
    // of the unsorted middle subarray
    int beginIndex = 0;
    int endIndex = N - 1;
    bool different = false;
    for (int i = N - 1; i > 0; --i) {
        if (minFromRight[i] == maxFromLeft[i]) {
            if (minFromRight[i - 1] != maxFromLeft[i - 1]) {
                endIndex = i - 1;
                break;
            }
        } else {
            different = true;
            break;
        }
    }
    if (!different) {
        return Indices(-1, -1); // Array is already sorted
    }
    for (int i = 0; i < N; ++i) {
        if (minFromRight[i] == maxFromLeft[i]) {
            if (minFromRight[i + 1] != maxFromLeft[i + 1]) {
                beginIndex = i + 1;
                break;
            }
        } else {
            break;
        }
    }
    return Indices(beginIndex, endIndex);
}

void test() {
    // Build randomly shuffled array
    int arr[N];
    for (int i = 0; i < N; ++i) {
        arr[i] = i;
    }
    shuffle(arr);
    auto indices = getSubarrayIndices(arr);
    // Before testing that sorting the subarray will sort the whole array
    // test with all smaller subarrays inside the current subarray
    auto endIndex = indices.endIndex;
    auto beginIndex = indices.beginIndex;
    if (indices.beginIndex == -1) {
        return;
    }
    for (int l = endIndex - beginIndex; l >= 2; --l) { // Smaller lengths
        for (int i = 0; i + l - 1 <= endIndex; ++i) {
            int copyArr[N];
            for (int i = 0; i < N; ++i) {
                copyArr[i] = arr[i];
            }
            auto end = i + l - 1;
            auto begin = beginIndex + i;
            qsort(begin, end, copyArr);
            // Make sure there is NO smaller subarray that would work
            // We're still in the bounds of our answer so we don't know if
            // a smaller subarray exists outside that would be better (however,
            // our assertion at the end will remove this possibility)
            assert(!isSorted(copyArr));
        }
    }
    qsort(beginIndex, endIndex, arr);
    assert(isSorted(arr));
}

int main() {
    srand(time(0));
    assert(N > 1);
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        test();
    }
    return 0;
}
