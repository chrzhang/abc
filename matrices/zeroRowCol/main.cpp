#include <iostream>
#include <ctime>
#include <cstdlib>

#define M 4
#define N 7

// Write an algorithm where if an element in a M x N matrix is 0, the row and
// column should be filled with 0s

void printMatrix(bool matrix[M][N]) {
    for (int i = 0; i < M; ++i) {
        for (int j = 0; j < N; ++j) {
            std::cout << matrix[i][j] << " ";
        }
        std::cout << std::endl;
    }
}

void zeroOut(bool matrix[M][N]) {
    // Keep track of where zeroes occur
    bool rows[M] = { 0 };
    bool cols[N] = { 0 };
    for (int i = 0; i < M; ++i) {
        for (int j = 0; j < N; ++j) {
            if (matrix[i][j] == 0) {
                rows[i] = 1;
                cols[j] = 1;
            }
        }
    }
    for (int i = 0; i < M; ++i) {
        if (rows[i]) {
            for (int k = 0; k < N; ++k) {
                matrix[i][k] = 0;
            }
        }
    }
    for (int j = 0; j < N; ++j) {
        if (cols[j]) {
            for (int k = 0; k < M; ++k) {
                matrix[k][j] = 0;
            }
        }
    }
}

int main() {
    bool matrix[M][N];
    bool possibilities[] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 0}; // p = 0.10
    srand(time(0));
    for (int i = 0; i < M; ++i) {
        for (int j = 0; j < N; ++j) {
            matrix[i][j] = possibilities[rand() % 10];
        }
    }
    printMatrix(matrix);
    std::cout << "Zero-ing out the matrix." << std::endl;
    zeroOut(matrix);
    printMatrix(matrix);
    return 0;
}
