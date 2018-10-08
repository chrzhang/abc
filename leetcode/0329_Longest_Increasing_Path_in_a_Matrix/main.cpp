class Solution {
public:
    void findLengthsAux(vector<vector<int>> & lengths, vector<vector<int>> & matrix, int row, int col) {
        int currVal = 1;
        // North
        if (row > 0 && matrix[row][col] > matrix[row - 1][col]) {
            if (lengths[row - 1][col] == 1) {
                findLengthsAux(lengths, matrix, row - 1, col);
            }
            currVal = max(currVal, 1 + lengths[row - 1][col]);
        }
        // East
        if (col < matrix[0].size() - 1 && matrix[row][col] > matrix[row][col + 1]) {
            if (lengths[row][col + 1] == 1) {
                findLengthsAux(lengths, matrix, row, col + 1);
            }
            currVal = max(currVal, 1 + lengths[row][col + 1]);
        }
        // West
        if (col > 0 && matrix[row][col] > matrix[row][col - 1]) {
            if (lengths[row][col - 1] == 1) {
                findLengthsAux(lengths, matrix, row, col - 1);
            }
            currVal = max(currVal, 1 + lengths[row][col - 1]);
        }
        // South
        if (row < matrix.size() - 1 && matrix[row][col] > matrix[row + 1][col]) {
            if (lengths[row + 1][col] == 1) {
                findLengthsAux(lengths, matrix, row + 1, col);
            }
            currVal = max(currVal, 1 + lengths[row + 1][col]);
        }
        lengths[row][col] = currVal;
        
    }
    void findLengths(vector<vector<int>> & lengths, vector<vector<int>> & matrix) {
        for (auto row = 0; row < matrix.size(); ++row) {
            for (auto col = 0; col < matrix[0].size(); ++col) {
                if (lengths[row][col] == 1) {
                    findLengthsAux(lengths, matrix, row, col);
                }
            }
        }
    }
    int longestIncreasingPath(vector<vector<int>>& matrix) {
        if (matrix.empty()) { return 0; }
        // Use DP to not recalculate everything
        vector<vector<int>> lengths (matrix.size(), vector<int>(matrix[0].size(), 1));
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
};