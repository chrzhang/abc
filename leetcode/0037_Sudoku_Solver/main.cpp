class Solution {
public:
struct Board {
    char b[9][9]; // '1' - '9' for entries, '.' for unknown
    Board(const vector<vector<char>> & in) {
        assert(in.size() == 9);
        for (size_t r = 0; r < 9; ++r) {
            assert(in[r].size() == 9);
            for (size_t c = 0; c < 9; ++c) {
                    assert(in[r][c] == '.' || (in[r][c] >= '1' && in[r][c] <= '9'));
                    b[r][c] = in[r][c];
            }
        }
    }
    void set(const int r, const int c, const char v) {
        assert((v >= '1' && v <= '9') || v == '.');
        b[r][c] = v;
    }
    pair<int, int> find_blank() const {
        for (size_t r = 0; r < 9; ++r) {
            for (size_t c = 0; c < 9; ++c) {
                if (b[r][c] == '.') {
                    return make_pair(r, c);
                }
            }
        }
        return make_pair(-1, -1);
    };
    void candidates(const int r, const int c, bool cands[9]) const {
        assert(b[r][c] == '.');
        for (size_t cc = 0; cc < 9; ++cc) {
            if (b[r][cc] != '.') { // Share row
                cands[b[r][cc] - '1'] = false;
            }
        }
        for (size_t rr = 0; rr < 9; ++rr) {
            if (b[rr][c] != '.') { // Share col
                cands[b[rr][c] - '1'] = false;
            }
        }
        const int xr = r - r % 3; // Top left of 3x3 square
        const int xc = c - c % 3;
        for (size_t rr = 0; rr < 3; ++rr) {
            for (size_t cc = 0; cc < 3; ++cc) {
                if (b[xr + rr][xc + cc] != '.') {
                    cands[b[xr + rr][xc + cc] - '1'] = false;
                }
            }
        }
    }
};

bool solve(Board b, Board & result) {
    pair<int, int> blank = b.find_blank();
    if (blank.first == -1) {
        result = b;
        return true;
    }
    bool cands[9];
    for (size_t i = 0; i < 9; ++i) {
        cands[i] = true;
    }
    b.candidates(blank.first, blank.second, cands);
    for (size_t i = 0; i < 9; ++i) {
        if (cands[i]) {
            const char c = i + '1';
            b.set(blank.first, blank.second, c);
            if (solve(b, result)) {
                return true;
            }
        }
    }
    b.set(blank.first, blank.second, '.');
    return false;
}

void solveSudoku(vector<vector<char>>& board) {
    Board b(board);
    Board result = b;
    solve(b, result);
    for (int r = 0; r < 9; ++r) {
        for (int c = 0; c < 9; ++c) {
            board[r][c] = result.b[r][c];
        }
    }
    
}
};