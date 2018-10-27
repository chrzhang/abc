class Solution {
public:

struct Cell {
    size_t row;
    size_t col;
    int val;
    Cell(size_t r, size_t c, int v) : row(r), col(c), val(v) {
    }
    bool operator<(const Cell & rhs) {
        return val > rhs.val;
    }
};

int kthSmallest(const std::vector<std::vector<int>> & matrix, size_t k) {
    std::vector<std::vector<bool>> visited(matrix.size(), std::vector<bool>(matrix[0].size(), false));
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
};