class Solution {
bool can_put_at(const int r, const int c, const vector<vector<bool>> & board) {
    const int N = board.size();
    assert(r >= 0 && r < N && c >= 0 && c < N);
    for (int i = 0; i < N; ++i) { // Check row and columns
        if (board[r][i]) { return false; }
        if (board[i][c]) { return false; }
    }
    { // Check NE
        int tr(r), tc(c);
        while (tr >= 0 && tc < N) {
            if (board[tr--][tc++]) { return false; }
        }
    }
    { // Check NW
        int tr(r), tc(c);
        while (tr >= 0 && tc >= 0) {
            if (board[tr--][tc--]) { return false; }
        }
    }
    { // Check SE
        int tr(r), tc(c);
        while (tr < N && tc < N) {
            if (board[tr++][tc++]) { return false; }
        }
    }
    { // Check SW
        int tr(r), tc(c);
        while (tr < N && tc >= 0) {
            if (board[tr++][tc--]) { return false; }
        }
    }
    return true;
}
public:
void solve(const int nq, // # queens placed
           vector<vector<bool>> & board,
           const int r, // row
           set<vector<vector<bool>>> & result) {
    const int N = board.size();
    if (N == nq) {
        result.insert(board);
        return;
    }
    for (int c = 0; c < N; ++c) {
        if (can_put_at(r, c, board)) {
            board[r][c] = true;
            solve(1 + nq, board, 1 + r, result);
            board[r][c] = false;
        }
    }
}

int totalNQueens(const int N) {
    set<vector<vector<bool>>> result;
    vector<vector<bool>> board(N, vector<bool>(N, false));
    solve(0, board, 0, result);
    return result.size();
}
};