class Solution {
bool can_put_at(const int r, const int c, const vector<string> & board) {
    const int N = board.size();
    assert(r >= 0 && r < N && c >= 0 && c < N);
    for (int i = 0; i < N; ++i) { // Check row and columns
        if (board[r][i] == 'Q') { return false; }
        if (board[i][c] == 'Q') { return false; }
    }
    { // Check NE
        int tr(r), tc(c);
        while (tr >= 0 && tc < N) {
            if (board[tr--][tc++] == 'Q') { return false; }
        }
    }
    { // Check NW
        int tr(r), tc(c);
        while (tr >= 0 && tc >= 0) {
            if (board[tr--][tc--] == 'Q') { return false; }
        }
    }
    { // Check SE
        int tr(r), tc(c);
        while (tr < N && tc < N) {
            if (board[tr++][tc++] == 'Q') { return false; }
        }
    }
    { // Check SW
        int tr(r), tc(c);
        while (tr < N && tc >= 0) {
            if (board[tr++][tc--] == 'Q') { return false; }
        }
    }
    return true;
}
public:
void solve(const int nq, // # queens placed
           vector<string> & board,
           const int r, // row
           vector<vector<string>> & result) {
    const int N = board.size();
    if (N == nq) {
        result.push_back(board);
        return;
    }
    for (int c = 0; c < N; ++c) {
        if (can_put_at(r, c, board)) {
            board[r][c] = 'Q';
            solve(1 + nq, board, 1 + r, result);
            board[r][c] = '.';
        }
    }
}

vector<vector<string>> solveNQueens(const int N) {
    vector<vector<string>> result;
    vector<string> board(N, string(N, '.'));
    solve(0, board, 0, result);
    return result;
}
};