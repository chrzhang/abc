#include <iostream>
#include <cstdlib>
#include <ctime>
#include <assert.h>

#define ARR_SIZE 10
#define NUM_ITERATIONS 1000000

// Find values that match their indices in a sorted set of numbers

int findMagicIndex(int arr[ARR_SIZE], int begin, int end) {
    int mid = (begin + end) / 2;
    if (arr[mid] == mid) {
        return mid;
    } else if (arr[mid] > mid) {
        return findMagicIndex(arr, begin, mid - 1);
    } else {
        return findMagicIndex(arr, mid + 1, end);
    }
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        int arr[ARR_SIZE];
        int magicIndex = rand() % ARR_SIZE;
        arr[magicIndex] = magicIndex;
        for (int i = magicIndex - 1; i >= 0; --i) {
            arr[i] = arr[i + 1] - (rand() % 5 + 1);
        }
        for (int i = magicIndex + 1; i < ARR_SIZE; ++i) {
            arr[i] = arr[i - 1] + (rand() % 5 + 1);
        }
        int mi = findMagicIndex(arr, 0, ARR_SIZE - 1);
        assert(arr[mi] == mi);
    }
    return 0;
}
