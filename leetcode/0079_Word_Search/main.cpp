class Solution {
public:
bool eaux(const vector<vector<char>> & board, const size_t r, const size_t c,
          const string & n, const int ni, vector<vector<bool>> & visited) {
    if (ni == n.size() - 1) { return true; }
    visited[r][c] = true;
    const auto H = board.size();
    const auto W = board[r].size();
    // North
    if (r > 0 && !visited[r - 1][c] && board[r - 1][c] == n[ni + 1]) {
        if (eaux(board, r - 1, c, n, ni + 1, visited)) { return true; }
    }
    // East
    if (c + 1 < W && !visited[r][c + 1] && board[r][c + 1] == n[ni + 1]) {
        if (eaux(board, r, c + 1, n, ni + 1, visited)) { return true; }
    }
    // West
    if (c > 0 && !visited[r][c - 1] && board[r][c - 1] == n[ni + 1]) {
        if (eaux(board, r, c - 1, n, ni + 1, visited)) { return true; }
    }
    // South
    if (r + 1 < H && !visited[r + 1][c] && board[r + 1][c] == n[ni + 1]) {
        if (eaux(board, r + 1, c, n, ni + 1, visited)) { return true; }
    }
    visited[r][c] = false;
    return false;
}

bool exist(const vector<vector<char>> & board, const string & n) {
    if (n.empty()) { return false; }
    for (size_t r = 0; r < board.size(); ++r) {
        for (size_t c = 0; c < board[r].size(); ++c) {
            if (board[r][c] == n[0]) {
                vector<vector<bool>> visited(board.size(), vector<bool>(board[r].size(), false));
                if (eaux(board, r, c, n, 0, visited)) {
                    return true;
                }
            }
        }
    }
    return false;
}
};