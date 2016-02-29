#include <iostream>
#include <vector>
#include <iomanip>

#define N 5

void printMatrix(int m[N][N]) {
    for (int row = 0; row < N; ++row) {
        for (int col = 0; col < N; ++col) {
            std::cout << std::setw(3) << m[row][col];
        }
        std::cout << std::endl;
    }
}

bool isValidIndex(int row, int col) {
    return (row >= 0 && row < N && col >= 0 && col < N);
}

void printLeftDownFrom(int row, int col, int m[N][N]) {
    std::cout << m[row][col] << " ";
    int tempRightRow = row + 1;
    int tempRightCol = col - 1;
    while (isValidIndex(tempRightRow, tempRightCol)) {
        //std::cout << ", (" << tempRightRow << "," << tempRightCol << ") ";
        std::cout << m[tempRightRow][tempRightCol] << " ";
        ++tempRightRow;
        --tempRightCol;
    }
    std::cout << "\n";
}

void printDiagonals(int m[N][N]) {
    int topRightRow, topRightCol;
    topRightRow = topRightCol = 0;
    while (isValidIndex(topRightRow, topRightCol)) { // Go right
        printLeftDownFrom(topRightRow, topRightCol, m);
        ++topRightCol;
    }
    --topRightCol;
    ++topRightRow;
    while (isValidIndex(topRightRow, topRightCol)) { // Go down
        printLeftDownFrom(topRightRow, topRightCol, m);
        ++topRightRow;
    }
}

int main() {
    int m[N][N];
    for (int row = 0; row < N; ++row) {
        for (int col = 0; col < N; ++col) {
            m[row][col] = row * N + col;
        }
    }
    printMatrix(m);
    printDiagonals(m);
    return 0;
}
