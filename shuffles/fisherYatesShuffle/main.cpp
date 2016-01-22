#include <iostream>
#include <iomanip>
#include <ctime>
#include <cstdlib>
#include <cassert>
#include <unordered_map>

#define N 4
#define NUM_SHUFFLES 1000000

// Perform a fair shuffle using the Fisher-Yates method

void swap(int & a, int & b) {
    auto temp = a;
    a = b;
    b = temp;
}

void shuffle(int arr[N]) {
    for (int i = 0; i < N; ++i) {
        // Pick a position to swap with
        auto pos = i + rand() % (N - i);
        swap(arr[i], arr[pos]);
    }
}

void printArray(int arr[N]) {
    for (int i = 0; i < N; ++i) {
        std::cout << arr[i] << " ";
    }
    std::cout << std::endl;
}

std::string toString(int arr[N]) {
    std::string s;
    for (int i = 0; i < N; ++i) {
        s += std::to_string(arr[i]);
    }
    return s;
}

int main() {
    srand(time(0));
    int arr[N];
    for (int i = 0; i < N; ++i) {
        arr[i] = i;
    }
    std::unordered_map<std::string, int> perms;
    for (int i = 0; i < NUM_SHUFFLES; ++i) {
        shuffle(arr);
        ++perms[toString(arr)];
    }
    for (auto it = perms.begin(); it != perms.end(); ++it) {
        std::cout << std::setw(5) << std::distance(perms.begin(), it) + 1
                  << ": " << it->first << "\t" << it->second
                  << "\t" << 100 * (((double) it->second) / NUM_SHUFFLES) << "%"
                  << std::endl;
    }
}
