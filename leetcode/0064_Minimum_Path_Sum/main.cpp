class Solution {
public:
    int minPathSum(vector<vector<int>>& grid) {
        int height = grid.size();
        int width = grid[0].size();
        vector<vector<int>> table = grid;
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
                table[row][col] += min(table[row - 1][col], table[row][col - 1]);
            }
        }
        return table[height - 1][width - 1];
    }
};