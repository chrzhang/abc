class Solution {
public:
    int uniquePathsWithObstacles(vector<vector<int>>& obstacleGrid) {
        auto width = obstacleGrid[0].size();
        auto height = obstacleGrid.size();
        vector<vector<int>> table(height, vector<int>(width, 0));
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
};