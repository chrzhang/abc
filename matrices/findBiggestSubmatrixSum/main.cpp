#include <iostream>
#include <iomanip>
#include <cassert>
#include <cstdlib>
#include <ctime>
#include <climits>

#define N 3

// Find submatrix with the biggest sum in a square matrix with positive
// and negative values

void printMatrix(int m[N][N]) {
    for (int row = 0; row < N; ++row) {
        for (int col = 0; col < N; ++col) {
            std::cout << std::setw(3) << m[row][col] << " ";
        }
        std::cout << std::endl;
    }
}

int sumOfSubmatrix(int nwRow, int nwCol, int seRow, int seCol, int m[N][N]) {
    int sum = 0;
    for (int row = nwRow; row <= seRow; ++row) {
        for (int col = nwCol; col <= seCol; ++col) {
            sum += m[row][col];
        }
    }
    return sum;
}

void findPrecomputedSums(int nwRow, int nwCol, int precomputedSums[N][N],
                         int m[N][N]) {
    for (int row = 0; row < N; ++row) {
        for (int col = 0; col < N; ++col) {
            precomputedSums[row][col] = 0;
        }
    }
    for (int row = nwRow; row < N; ++row) {
        for (int col = nwCol; col < N; ++col) {
            if (row == nwRow && col == nwCol) {
                precomputedSums[row][col] = m[row][col];
            } else if (row == nwRow) {
                precomputedSums[row][col] = precomputedSums[row][col - 1] +
                                            m[row][col];
            } else if (col == nwCol) {
                precomputedSums[row][col] = precomputedSums[row - 1][col] +
                                            m[row][col];
            } else {
                int sum = precomputedSums[row - 1][col];
                for (int scol = nwCol; scol <= col; ++scol) {
                    sum += m[row][scol];
                }
                precomputedSums[row][col] = sum;
            }
        }
    }
}

void findSubmatrixWithBiggestSum(int m[N][N]) {
    auto currentMax = INT_MIN;
    auto topLeftRow = -1;
    auto topLeftCol = -1;
    auto botRightRow = -1;
    auto botRightCol = -1;
    for (int nwRow = 0; nwRow < N; ++nwRow) {
        for (int nwCol = 0; nwCol < N; ++nwCol) {
            // Pre-compute sums for every submatrix from the top left
            int precomputedSums[N][N];
            findPrecomputedSums(nwRow, nwCol, precomputedSums, m);
            for (int seRow = nwRow; seRow < N; ++seRow) {
                for (int seCol = nwCol; seCol < N; ++seCol) {
                    //auto s = sumOfSubmatrix(nwRow, nwCol, seRow, seCol, m);
                    //assert(s == precomputedSums[seRow][seCol]);
                    auto s = precomputedSums[seRow][seCol];
                    if (s > currentMax) {
                        currentMax = s;
                        topLeftRow = nwRow;
                        topLeftCol = nwCol;
                        botRightRow = seRow;
                        botRightCol = seCol;
                    }
                }
            }
        }
    }
    std::cout << "The sub-matrix with the biggest sum of " << currentMax
              << " has top-left corner at " << topLeftRow << ", " << topLeftCol
              << " and bottom-right corner at " << botRightRow << ", "
              << botRightCol << std::endl;
}

int main() {
    srand(time(0));
    int m[N][N];
    for (int row = 0; row < N; ++row) {
        for (int col = 0; col < N; ++col) {
            m[row][col] = (rand() % 5) - 2;
        }
    }
    printMatrix(m);
    findSubmatrixWithBiggestSum(m);
    return 0;
}
