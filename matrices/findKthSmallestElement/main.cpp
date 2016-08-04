#include <iostream>
#include <vector>
#include <stdexcept>
#include <algorithm>

// Given an ascending N x N matrix, find the kth smallest element

bool isSquare(const std::vector<std::vector<int>> & matrix) {
    for (auto row : matrix) {
        if (row.size() != matrix.size()) {
            return false;
        }
    }
    return true;
}

struct Cell {
    size_t row;
    size_t col;
    int val;
    Cell(size_t r, size_t c, int v) : row(r), col(c), val(v) {
    }
};

bool operator<(const Cell & lhs, const Cell & rhs) {
    return lhs.val > rhs.val;
}

std::ostream & operator<<(std::ostream & os, const Cell & cell) {
    os << "(" << cell.row << ", " << cell.col << ") = " << cell.val;
    return os;
}

int kthSmallest(const std::vector<std::vector<int>> & matrix, size_t k) {
    if (!isSquare(matrix) || k < 1 || k > matrix.size() * matrix.size() ||
        matrix.empty()) {
        throw std::runtime_error("Invalid parameters.");
    }
    std::vector<std::vector<bool>> visited(matrix.size(),
                                           std::vector<bool>(matrix[0].size(),
                                                             false));
    std::vector<Cell> frontier;
    frontier.push_back(Cell(0, 0, matrix[0][0]));
    Cell result(0, 0, 0);
    for (size_t i = 0; i < k; ++i) {
        result = frontier.front();
        std::pop_heap(frontier.begin(), frontier.end());
        frontier.pop_back();
        if (result.row + 1 < matrix.size() &&
            !visited[result.row + 1][result.col]) {
            frontier.push_back(Cell(result.row + 1, result.col,
                                    matrix[result.row + 1][result.col]));
            std::push_heap(frontier.begin(), frontier.end());
            visited[result.row + 1][result.col] = true;
        }
        if (result.col + 1 < matrix.size() &&
            !visited[result.row][result.col + 1]) {
            frontier.push_back(Cell(result.row, result.col + 1,
                                    matrix[result.row][result.col + 1]));
            std::push_heap(frontier.begin(), frontier.end());
            visited[result.row][result.col + 1] = true;
        }
    }
    return result.val;
}

int main() {
    std::vector<std::vector<int>> matrix = { { 1,  3,  5},
                                             { 6,  7, 12},
                                             {11, 14, 14} };
    for (size_t i = 1; i <= matrix.size() * matrix.size(); ++i) {
        std::cout << kthSmallest(matrix, i) << std::endl;
    }
    return 0;
}
