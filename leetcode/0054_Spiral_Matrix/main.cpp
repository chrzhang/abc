class Solution {
public:
    vector<int> spiralOrder(vector<vector<int>>& m)
    {
        if (m.empty()) {
            return vector<int>();
        }
        vector<vector<bool>> visited(m.size(), vector<bool>(m[0].size(), false));
        visited[0][0] = true;
        int r = 0;
        int c = 0;
        const int NC = m[0].size();
        const int NR = m.size();
        vector<int> result;
        result.push_back(m[r][c]);
        for (;;) {
            while (c + 1 < NC && !visited[r][c + 1]) {
                ++c;
                result.push_back(m[r][c]);
                visited[r][c] = true;
            }
            while (r + 1 < NR && !visited[r + 1][c]) {
                ++r;
                result.push_back(m[r][c]);
                visited[r][c] = true;
            }
            while (c - 1 >= 0 && !visited[r][c - 1]) {
                --c;
                result.push_back(m[r][c]);
                visited[r][c] = true;
            }
            while (r - 1 >= 0 && !visited[r - 1][c]) {
                --r;
                result.push_back(m[r][c]);
                visited[r][c] = true;
            }
            if (result.size() == NC * NR) {
                return result;
            }
        }
    }
};