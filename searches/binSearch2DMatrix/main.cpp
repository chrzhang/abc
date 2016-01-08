#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <ctime>
#include <assert.h>

#define WIDTH 10
#define HEIGHT 5

// Find an element in a 2D sorted (NW to SE) matrix

struct Position {
    int row;
    int col;
    Position(int r, int c) : row(r), col(c) {}
};

int max(int a, int b) {
    return a > b ? a : b;
}

void print(int matrix[HEIGHT][WIDTH]) {
    std::cout << std::setw(4) << "";
    for (int i = 0; i < WIDTH; ++i) {
        std::cout << std::setw(4) << i;
    }
    std::cout << std::endl;
    for (int row = 0; row < HEIGHT; ++row) {
        std::cout << std::setw(4) << row;
        for (int col = 0; col < WIDTH; ++col) {
            std::cout << std::setw(4) << matrix[row][col];
        }
        std::cout << std::endl;
    }
}

Position search(const int & e, int matrix[HEIGHT][WIDTH]) {
    // Start at the top right
    Position p(0, WIDTH - 1);
    while (p.col >= 0 && p.row >= 0) {
        if (e < matrix[p.row][p.col]) {
            --p.col; // Go left
        } else if (e > matrix[p.row][p.col]) {
            ++p.row; // Go down
        } else {
            return p;
        }
    }
    return Position(-1, -1);
}

int main() {
    srand(time(0));
    int matrix[HEIGHT][WIDTH];
    for (int row = 0; row < HEIGHT; ++row) {
        for (int col = 0; col < WIDTH; ++col) {
            if (0 == row && 0 == col) {
                matrix[row][col] = 0;
            } else if (0 == row) {
                matrix[row][col] = matrix[row][col - 1] + (rand() % 5 + 1);
            } else if (0 == col) {
                matrix[row][col] = matrix[row - 1][col] + (rand() % 5 + 1);
            } else {
                matrix[row][col] = max(matrix[row][col - 1],
                                       matrix[row - 1][col]) + (rand() % 5 + 1);
            }
        }
    }
    print(matrix);
    for (int row = 0; row < HEIGHT; ++row) {
        for (int col = 0; col < WIDTH; ++col) {
            auto found = search(matrix[row][col], matrix);
            assert(matrix[row][col] == matrix[found.row][found.col]);
        }
    }
    return 0;
}
