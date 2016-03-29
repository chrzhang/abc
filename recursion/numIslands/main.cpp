#include <iostream>
#include <cstdlib>
#include <ctime>

#define HEIGHT 5
#define WIDTH 5
#define LAND 1
#define WATER 0

// Count the number of islands in a binary matrix of land (1) and water (0)

void print(const bool world[HEIGHT][WIDTH]) {
    std::cout << "World:\n";
    for (int row = 0; row < HEIGHT; ++row) {
        for (int col = 0; col < WIDTH; ++col) {
            std::cout << (world[row][col] == LAND ? "o" : "-") << " ";
        }
        std::cout << "\n";
    }
}

void fill(int row, int col, bool visited[HEIGHT][WIDTH],
          const bool world[HEIGHT][WIDTH]) {
    if (row < 0 || row >= HEIGHT || col < 0 || col >= WIDTH) { return; }
    visited[row][col] = true;
    // Check north, east, west, south for unvisited lands
    if (row > 0) {
        if (!visited[row - 1][col] && world[row - 1][col] == LAND) {
            fill(row - 1, col, visited, world);
        }
    }
    if (col < WIDTH - 1) {
        if (!visited[row][col + 1] && world[row][col + 1] == LAND) {
            fill(row, col + 1, visited, world);
        }
    }
    if (col > 0) {
        if (!visited[row][col - 1] && world[row][col - 1] == LAND) {
            fill(row, col - 1, visited, world);
        }
    }
    if (row < HEIGHT - 1) {
        if (!visited[row + 1][col] && world[row + 1][col] == LAND) {
            fill(row + 1, col, visited, world);
        }
    }
}

int countIslands(const bool world[HEIGHT][WIDTH]) {
    int numIslands = 0;
    bool visited[HEIGHT][WIDTH] = { { false } };
    for (int row = 0; row < HEIGHT; ++row) {
        for (int col = 0; col < WIDTH; ++col) {
            if (visited[row][col]) { continue; }
            visited[row][col] = true;
            if (world[row][col] == LAND) {
                ++numIslands;
                fill(row, col, visited, world);
            }
        }
    }
    return numIslands;
}

int main() {
    srand(time(0));
    bool world[HEIGHT][WIDTH];
    for (int row = 0; row < HEIGHT; ++row) {
        for (int col = 0; col < WIDTH; ++col) {
            world[row][col] = rand() % 2;
        }
    }
    print(world);
    auto n = countIslands(world);
    std::cout << "# islands: " << n << "\n";
    return 0;
}
