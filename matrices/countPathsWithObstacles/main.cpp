#include <iostream>
#include <vector>

// Starting from the top left unit of a matrix, count the ways to reach the
// bottom right unit by going only right or down and avoiding un-traversable
// cells (marked as 1s)

void printGrid(const std::vector<std::vector<int>> & g) {
    for (auto row : g) {
        for (auto e : row) {
            std::cout << e << " ";
        }
        std::cout << "\n";
    }
}

int uniquePathsWithObstacles(std::vector<std::vector<int>>& obstacleGrid) {
    auto width = obstacleGrid[0].size();
    auto height = obstacleGrid.size();
    std::vector<std::vector<int>> table(height, std::vector<int>(width, 0));
    // Initialize the rightmost col and lowest row
    bool foundObstacle = false;
    for (int row = height - 1; row >= 0; --row) {
        if (foundObstacle) {
            table[row][width - 1] = 0;
        } else {
            if (obstacleGrid[row][width - 1] == 1) {
                table[row][width - 1] = 0;
                foundObstacle = true;
            } else {
                table[row][width - 1] = 1;
            }
        }
    }
    foundObstacle = false;
    for (int col = width - 1; col >= 0; --col) {
        if (foundObstacle) {
            table[height - 1][col] = 0;
        } else {
            if (obstacleGrid[height - 1][col] == 1) {
                table[height - 1][col] = 0;
                foundObstacle = true;
            } else {
                table[height - 1][col] = 1;
            }
        }
    }
    // Iterate bottom up, right to left (alternative to recursion)
    for (int row = height - 2; row >= 0; --row) {
        for (int col = width - 2; col >= 0; --col) {
            if (obstacleGrid[row][col] == 1) {
                table[row][col] = 0;
            } else {
                table[row][col] = table[row + 1][col] + table[row][col + 1];
            }
        }
    }
    return table[0][0];
}

int main() {
    std::vector<std::vector<int>> obstacleGrid = { { 0, 0, 0 },
                                                   { 0, 1, 0 },
                                                   { 0, 0, 0 } };
    printGrid(obstacleGrid);
    std::cout << "Ways: " << uniquePathsWithObstacles(obstacleGrid) << "\n";
    return 0;
}
