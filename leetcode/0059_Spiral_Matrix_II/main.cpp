class Solution {
public:
vector<vector<int>> generateMatrix(const int N) {
    vector<vector<int>> m(N, vector<int>(N, -1));
    int n(2), r(0), c(0);
    m[r][c] = 1;
    while (n <= N * N) {
        while (c + 1 < N && m[r][c + 1] == -1) {
            m[r][c++ + 1] = n++;
        }
        while (r + 1 < N && m[r + 1][c] == -1) {
            m[r++ + 1][c] = n++;
        }
        while (c - 1 >= 0 && m[r][c - 1] == -1) {
            m[r][c-- - 1] = n++;
        }
        while (r - 1 >= 0 && m[r - 1][c] == -1) {
            m[r-- - 1][c] = n++;
        }
    }
    return m;
}
};