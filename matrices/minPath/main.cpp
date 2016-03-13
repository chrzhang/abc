#include <iostream>
#include <vector>
#include <cassert>

// Find smallest summing path through maze in a top-left to bottom-right
// traversal

void print(const std::vector<std::vector<int>> & grid) {
    for (auto row : grid) {
        for (auto e : row) {
            std::cout << e << " ";
        }
        std::cout << "\n";
    }
}

int minPathSum(const std::vector<std::vector<int>> & grid) {
    int height = grid.size();
    int width = grid[0].size();
    // Use dynamic programming, a path to a block is the smaller of summing the
    // block with the minimum sums of paths to the block's north and west
    // neighbors
    std::vector<std::vector<int>> table = grid;
    // Initialize top row, left col
    int sum = 0;
    for (size_t col = 0; col < width; ++col) {
        sum += table[0][col];
        table[0][col] = sum;
    }
    sum = 0;
    for (size_t row = 0; row < height; ++row) {
        sum += table[row][0];
        table[row][0] = sum;
    }
    for (size_t row = 1; row < height; ++row) {
        for (size_t col = 1; col < width; ++col) {
            table[row][col] += std::min(table[row - 1][col],
                                        table[row][col - 1]);
        }
    }
    return table[height - 1][width - 1];
}

int main() {
    std::vector<std::vector<int>> grid = { {1, 2},
                                           {3, 0} };
    assert(minPathSum(grid) == 3);
    return 0;
}
