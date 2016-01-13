#include <iostream>
#include <assert.h>
#include <cstdlib>
#include <ctime>

#define N 10

// Find sum of a maximum subarray

void printArr(int arr[N]) {
    for (int i = 0; i < N; ++i) {
        std::cout << arr[i] << " ";
    }
    std::cout << std::endl;
}

int max(int a, int b) { return a > b ? a : b; }

int findMaxSum(int arr[N]) {
    int maximumSumEndingAtCurrentIndex = 0;
    int maximumSumSoFar = 0; // Keep track of biggest max ever found
    for (int i = 0; i < N; ++i) {
        // Either the maximum subarray is an extension or a new array
        maximumSumEndingAtCurrentIndex =
            max(0, maximumSumEndingAtCurrentIndex + arr[i]);
        if (maximumSumEndingAtCurrentIndex > maximumSumSoFar) {
            maximumSumSoFar = maximumSumEndingAtCurrentIndex;
        }
    }
    assert(maximumSumSoFar >= 0);
    return maximumSumSoFar;
}

int main() {
    srand(time(0));
    int arr[N];
    for (int i = 0; i < N; ++i) {
        arr[i] = rand() % 11 - 5;
    }
    printArr(arr);
    std::cout << "Maximum sum: " << findMaxSum(arr) << std::endl;
    return 0;
}
