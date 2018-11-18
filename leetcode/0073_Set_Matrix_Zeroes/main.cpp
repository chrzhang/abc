class Solution {
public:
    void setZeroes(vector<vector<int>>& m)
    {
        const auto H = m.size();
        const auto W = m[0].size();
        vector<bool> rows_zeroed(H, false);
        vector<bool> cols_zeroed(W, false);
        for (auto r = 0; r < H; ++r) {
            for (auto c = 0; c < W; ++c) {
                if (m[r][c] == 0) {
                    rows_zeroed[r] = cols_zeroed[c] = true;
                }
            }
        }
        for (auto r = 0; r < H; ++r) {
            if (rows_zeroed[r]) {
                for (auto c = 0; c < W; ++c) {
                    m[r][c] = 0;
                }
            }
        }
        for (auto c = 0; c < W; ++c) {
            if (cols_zeroed[c]) {
                for (auto r = 0; r < H; ++r) {
                    m[r][c] = 0;
                }
            }
        }
    }
};