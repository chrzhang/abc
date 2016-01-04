#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <ctime>
#include <assert.h>

#define WIDTH 20
#define HEIGHT 10
#define NUM_ITERATIONS 1

// Use bucket flood on a given pixel to color it

enum Color { red, green, blue };

void print(Color screen[HEIGHT][WIDTH]) {
    for (int row = 0; row < HEIGHT; ++row) {
        for (int col = 0; col < WIDTH; ++col) {
            if (screen[row][col] == red) {
                std::cout << "\033[1;31m█\033[0m";
            } else if (screen[row][col] == green) {
                std::cout << "\033[1;32m█\033[0m";
            } else if (screen[row][col] == blue) {
                std::cout << "\033[1;34m█\033[0m";
            } else {
                std::cout << "Unknown color.\n";
            }
        }
        std::cout << std::endl;
    }
}

// Build a blob of color
void randomlyFillOutwards(int row, int col, int & pixelsLeft,
                          Color c, Color screen[HEIGHT][WIDTH]) {
    if (pixelsLeft == 0) { return; }
    screen[row][col] = c;
    --pixelsLeft;
    if (row > 0 && screen[row - 1][col] == blue) { // N
        randomlyFillOutwards(row - 1, col, pixelsLeft, c, screen);
    }
    if (col < WIDTH - 1 && screen[row][col + 1] == blue) { // E
        randomlyFillOutwards(row, col + 1, pixelsLeft, c, screen);
    }
    if (row < HEIGHT - 1 && screen[row + 1][col] == blue) { // S
        randomlyFillOutwards(row + 1, col, pixelsLeft, c, screen);
    }
    if (col > 0 && screen[row][col - 1] == blue) { // W // W
        randomlyFillOutwards(row, col - 1, pixelsLeft, c, screen);
    }
    return;
}

void bucketFill(int row, int col, Color c, Color screen[HEIGHT][WIDTH]) {
    Color formerColor = screen[row][col];
    if (formerColor == c) { return; }
    screen[row][col] = c;
    if (row > 0 && screen[row - 1][col] == formerColor) { // N
        bucketFill(row - 1, col, c, screen);
    }
    if (col < WIDTH - 1 && screen[row][col + 1] == formerColor) { // E
        bucketFill(row, col + 1, c, screen);
    }
    if (row < HEIGHT - 1 && screen[row + 1][col] == formerColor) { // S
        bucketFill(row + 1, col, c, screen);
    }
    if (col > 0 && screen[row][col - 1] == formerColor) { // W
        bucketFill(row, col - 1, c, screen);
    }
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        Color screen[HEIGHT][WIDTH];
        // Max size for each island
        int sizeOfRedIsland = rand() % ((WIDTH * HEIGHT) / 2);
        int sizeOfGreenIsland = rand() % ((WIDTH * HEIGHT) / 2);
        while (sizeOfRedIsland == 0) {
            sizeOfRedIsland = rand() % ((WIDTH * HEIGHT) / 2);
        }
        while (sizeOfGreenIsland == 0) {
            sizeOfGreenIsland = rand() % ((WIDTH * HEIGHT) / 2);
        }
        for (int row = 0; row < HEIGHT; ++row) {
            for (int col = 0; col < WIDTH; ++col) {
                screen[row][col] = blue;
            }
        }
        // Random points of origin for islands
        int redOriginRow, redOriginCol;
        redOriginRow = rand() % HEIGHT;
        redOriginCol = rand() % WIDTH;
        randomlyFillOutwards(redOriginRow, redOriginCol, sizeOfRedIsland, red,
                             screen);
        int greenOriginRow, greenOriginCol;
        greenOriginRow = rand() % HEIGHT;
        greenOriginCol = rand() % WIDTH;
        while (screen[greenOriginRow][greenOriginCol] != blue) {
            greenOriginRow = rand() % HEIGHT;
            greenOriginCol = rand() % WIDTH;
        }
        randomlyFillOutwards(greenOriginRow, greenOriginCol, sizeOfGreenIsland,
                             green, screen);
        print(screen);
        std::cout << "Flooding all red with green.\n";
        // Build a copy for testing bucket flood
        bucketFill(redOriginRow, redOriginCol, green, screen);
        print(screen);
        std::cout << "Flooding all green with blue.\n";
        bucketFill(greenOriginRow, greenOriginCol, blue, screen);
        // Edge case of isolated island
        bucketFill(redOriginRow, redOriginCol, blue, screen);
        print(screen);
        for (int row = 0; row < HEIGHT; ++row) {
            for (int col = 0; col < WIDTH; ++col) {
                assert(screen[row][col] == blue);
            }
        }
    }
}
