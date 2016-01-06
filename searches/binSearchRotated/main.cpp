#include <iostream>
#include <cstdlib>
#include <ctime>
#include <assert.h>

#define SIZE 10
#define NUM_ITERATIONS 1000000

// Find an element in a sorted but rotated array

void printArr(int arr[SIZE]) {
    for (int i = 0; i < SIZE; ++i) {
        std::cout << arr[i] << " ";
    }
    std::cout << std::endl;
}

int indexBounds(int i) {
    if (i < 0) { return 0; }
    if (i >= SIZE) { return SIZE - 1; }
    return i;
}

size_t findStartIndexAux(int arr[SIZE], size_t begin, size_t end) {
    if (begin > end) { return begin; }
    if (arr[begin] <= arr[end]) {
        auto beforeBegin = indexBounds(begin - 1);
        auto pastEnd = indexBounds(end + 1);
        auto minIndex = (arr[beforeBegin] < arr[begin]) ?  beforeBegin : begin;
        return (arr[minIndex] < arr[pastEnd]) ?  minIndex : pastEnd;
    } else { // Pivot is in between
        return findStartIndexAux(arr, begin + 1, end - 1);
    }
}

size_t findStartIndex(int arr[SIZE]) {
    return findStartIndexAux(arr, 0, SIZE - 1);
}

int binSearch(int e, int arr[SIZE], size_t begin, size_t end) {
    if (begin > end) { return -1; }
    auto mid = (begin + end) / 2;
    if (e == arr[mid]) {
        return mid;
    } else if (e > arr[mid]) {
        return binSearch(e, arr, mid + 1, end);
    } else {
        return binSearch(e, arr, begin, end - 1);
    }
}

int findIndexOf(int e, int arr[SIZE]) {
    auto startIndex = findStartIndex(arr);
    if (arr[startIndex] == e) { return startIndex; }
    if (arr[0] == e) { return 0; }
    if (arr[SIZE - 1] == e) { return SIZE - 1; }
    if (arr[startIndex - 1] == e) { return startIndex - 1; }
    if (e > arr[startIndex] && e < arr[SIZE - 1]) {
        return binSearch(e, arr, startIndex + 1, SIZE - 2);
    } else if (e > arr[0] && e < arr[startIndex - 1]) {
        return binSearch(e, arr, 1, startIndex - 1);
    }
    return -1;
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        // Rotate array randomly
        int arr[SIZE];
        auto rotateAmount = rand() % SIZE;
        for (int i = 0; i < rotateAmount; ++i) {
            arr[i] = SIZE - (rotateAmount - i);
        }
        for (int i = rotateAmount; i < SIZE; ++i) {
            arr[i] = i - rotateAmount;
        }
        assert(0 == arr[findStartIndex(arr)]);
        for (int i = 0; i < SIZE; ++i) {
            assert(arr[findIndexOf(i, arr)] == i);
        }
    }
}
