class Solution {
public:
    bool isValidSudoku(vector<vector<char>>& board) {
        unordered_map<char, int> vals = {{'1', 0}, {'2', 1}, {'3', 2}, {'4', 3}, {'5', 4}, {'6', 5}, {'7', 6}, {'8', 7}, {'9', 8}};
        // Check each row
        for (auto row : board) {
            bool a[9] = { false };
            for (auto e : row) {
                if (e == '.') { continue; }
                if (a[vals[e]]) { return false; }
                a[vals[e]] = true;
            }
        }
        // Check each col
        for (int col = 0; col < 9; ++ col) {
            bool a[9] = { false };
            for (auto row : board) {
                if (row[col] == '.') { continue; }
                if (a[vals[row[col]]]) { return false; }
                a[vals[row[col]]] = true;
            }
        }
        // Check each 3 x 3 box
        for (int boxRow = 0; boxRow < 3; ++boxRow) {
            for (int boxCol = 0; boxCol < 3; ++boxCol) {
                bool a[9] = { false };
                for (int row = boxRow * 3; row < boxRow * 3 + 3; ++row) {
                    for (int col = boxCol * 3; col < boxCol * 3 + 3; ++col) {
                        if (board[row][col] == '.') { continue; }
                        if (a[vals[board[row][col]]]) { return false; }
                        a[vals[board[row][col]]] = true;
                    }
                }
            }
        }
        return true;
    }
};