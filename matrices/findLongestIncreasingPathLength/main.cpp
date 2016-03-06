#include <iostream>
#include <vector>
#include <algorithm>
#include <climits>
#include <cassert>

// Get length of the longest increasing path in a matrix

void findLengthsAux(std::vector<std::vector<int>> & lengths,
                    std::vector<std::vector<int>> & matrix,
                    size_t row, size_t col) {
    int currVal = 1;
    // North
    if (row > 0 && matrix[row][col] > matrix[row - 1][col]) {
        if (lengths[row - 1][col] == 1) {
            findLengthsAux(lengths, matrix, row - 1, col);
        }
        currVal = std::max(currVal, 1 + lengths[row - 1][col]);
    }
    // East
    if (col < matrix[0].size() - 1 && matrix[row][col] > matrix[row][col + 1]) {
        if (lengths[row][col + 1] == 1) {
            findLengthsAux(lengths, matrix, row, col + 1);
        }
        currVal = std::max(currVal, 1 + lengths[row][col + 1]);
    }
    // West
    if (col > 0 && matrix[row][col] > matrix[row][col - 1]) {
        if (lengths[row][col - 1] == 1) {
            findLengthsAux(lengths, matrix, row, col - 1);
        }
        currVal = std::max(currVal, 1 + lengths[row][col - 1]);
    }
    // South
    if (row < matrix.size() - 1 && matrix[row][col] > matrix[row + 1][col]) {
        if (lengths[row + 1][col] == 1) {
            findLengthsAux(lengths, matrix, row + 1, col);
        }
        currVal = std::max(currVal, 1 + lengths[row + 1][col]);
    }
    lengths[row][col] = currVal;
}

void findLengths(std::vector<std::vector<int>> & lengths,
                 std::vector<std::vector<int>> & matrix) {
    for (size_t row = 0; row < matrix.size(); ++row) {
        for (size_t col = 0; col < matrix[0].size(); ++col) {
            if (lengths[row][col] == 1) {
                findLengthsAux(lengths, matrix, row, col);
            }
        }
    }
}

void print(std::vector<std::vector<int>> & m) {
    for (auto row : m) {
        for (auto e : row) {
            std::cout << e << " ";
        }
        std::cout << "\n";
    }
}

int longestIncreasingPath(std::vector<std::vector<int>> & matrix) {
    if (matrix.empty()) { return 0; }
    // Use DP to not recalculate everything
    std::vector<std::vector<int>> lengths(
        matrix.size(), std::vector<int>(matrix[0].size(), 1));
    findLengths(lengths, matrix);
    int maxSoFar = INT_MIN;
    for (auto row : lengths) {
        for (auto e : row) {
            if (e > maxSoFar) {
                maxSoFar = e;
            }
        }
    }
    return maxSoFar;
}

int main() {
    std::vector<std::vector<int>> m {{9, 9, 4},
                                     {6, 6, 8},
                                     {2, 1, 1}};
    assert(longestIncreasingPath(m) == 4); // 1 -> 2 -> 6 -> 9
    m = {{3, 4, 5},
         {3, 2, 6},
         {2, 2, 1}};
    assert(longestIncreasingPath(m) == 4); // 3 -> 4 -> 5 -> 6
    return 0;
}
