class Solution {
public:
    int findNumNeighbors(const vector<vector<int>>& board, int rowIndex, int colIndex)
    {
        int width = board[0].size();
        // Dead cell with 3 live neighbors is born
        int numNeighbors = 0;
        // North
        if (rowIndex > 0) {
            if (board[rowIndex - 1][colIndex]) {
                ++numNeighbors;
            }
            if (colIndex < width - 1) { // Northeast
                if (board[rowIndex - 1][colIndex + 1]) {
                    ++numNeighbors;
                }
            }
            if (colIndex > 0) { // Northwest
                if (board[rowIndex - 1][colIndex - 1]) {
                    ++numNeighbors;
                }
            }
        }
        // East
        if (colIndex < width - 1) {
            if (board[rowIndex][colIndex + 1]) {
                ++numNeighbors;
            }
        }
        // West
        if (colIndex > 0) {
            if (board[rowIndex][colIndex - 1]) {
                ++numNeighbors;
            }
        }
        // South
        if (rowIndex < board.size() - 1) {
            if (board[rowIndex + 1][colIndex]) {
                ++numNeighbors;
            }
            if (colIndex < width - 1) { // Southeast
                if (board[rowIndex + 1][colIndex + 1]) {
                    ++numNeighbors;
                }
            }
            if (colIndex > 0) { // Southwest
                if (board[rowIndex + 1][colIndex - 1]) {
                    ++numNeighbors;
                }
            }
        }
        return numNeighbors;
    }
    void gameOfLife(vector<vector<int>>& board)
    {
        list<pair<int, int>> birthList;
        list<pair<int, int>> deathList;
        for (int rowIndex = 0; rowIndex < board.size(); ++rowIndex) {
            for (int colIndex = 0; colIndex < board[rowIndex].size(); ++colIndex) {
                auto nn = findNumNeighbors(board, rowIndex, colIndex);
                if (board[rowIndex][colIndex]) {
                    if (nn < 2) {
                        deathList.push_back(pair<int, int>(rowIndex, colIndex));
                    } else if (3 == nn || 2 == nn) {
                        // Lives on, do nothing
                    } else if (nn > 3) {
                        deathList.push_back(pair<int, int>(rowIndex, colIndex));
                    }
                } else {
                    if (3 == nn) {
                        birthList.push_back(pair<int, int>(rowIndex, colIndex));
                    }
                }
            }
        }
        for (auto p : birthList) {
            board[p.first][p.second] = 1;
        }
        for (auto p : deathList) {
            board[p.first][p.second] = 0;
        }
    }
};