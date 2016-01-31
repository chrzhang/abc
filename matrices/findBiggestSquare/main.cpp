#include <iostream>
#include <cstdlib>
#include <ctime>

// Given a matrix, find the biggest possible square outline made of 1s

#define HEIGHT 4
#define WIDTH 4

int min(int a, int b) {
    return a < b ? a : b;
}

void printScreen(bool screen[HEIGHT][WIDTH]) {
    for (int row = 0; row < HEIGHT; ++row) {
        for (int col = 0; col < WIDTH; ++col) {
            std::cout << (screen[row][col] ? "* " : "  ");
        }
        std::cout << std::endl;
    }
}

bool isTopLeftOfSquare(int row, int col, int dim, bool screen[HEIGHT][WIDTH]) {
    if (dim <= 0) { return false; }
    if (dim > WIDTH || dim > HEIGHT) { return false; }
    // Top row and bottom row
    for (int i = 0; i < dim; ++i) {
        if (!screen[row][col + i]) { return false; }
        if (!screen[row + dim - 1][col + i]) { return false; }
    }
    // And rows in between
    for (int rowInBetween = row + 1; rowInBetween < row + dim - 1;
         ++rowInBetween) {
        if (!screen[rowInBetween][col] ||
            !screen[rowInBetween][col + dim - 1]) {
            return false;
        }
    }
    std::cout << dim << " x " << dim << " square found with upper left corner "
              << "at " << row << ", " << col << std::endl;
    return true;
}

bool checkForSquaresOfSize(int dim, bool screen[HEIGHT][WIDTH]) {
    if (dim <= 0) { return false; }
    if (dim > WIDTH || dim > HEIGHT) { return false; }
    for (int row = 0; row + (dim - 1) <= HEIGHT - 1; ++row) {
        for (int col = 0; col + (dim - 1) <= WIDTH - 1; ++col) {
            // Top left of potential square at (row, col)
            if (isTopLeftOfSquare(row, col, dim, screen)) {
                return true;
            }
        }
    }
    return false;
}

int main() {
    srand(time(0));
    bool screen[HEIGHT][WIDTH];
    for (int row = 0; row < HEIGHT; ++row) {
        for (int col = 0; col < WIDTH; ++col) {
            screen[row][col] = rand() % 2 ? true : false;
        }
    }
    printScreen(screen);
    for (int currDim = min(HEIGHT, WIDTH); currDim >= 1; --currDim) {
        if (checkForSquaresOfSize(currDim, screen)) {
            break;
        }
    }
    return 0;
}
