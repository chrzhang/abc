class Solution {
public:
    int min3(const int a, const int b, const int c)
    {
        return min({ a, b, c });
    }
    int minDistance(const string& a, const string& b)
    {
        // Wagner-Fischer algorithm
        // Uses DP to store edit distance of every prefix
        vector<vector<int>> dp(a.size() + 1, vector<int>(b.size() + 1, 0));
        for (auto i = 0; i < a.size() + 1; ++i) {
            dp[i][0] = i; // Empty to string of size N requires N additions
        }
        for (auto j = 0; j < b.size() + 1; ++j) {
            dp[0][j] = j; // String of size N to empty requires N deletions
        }
        for (auto r = 1; r < a.size() + 1; ++r) {
            for (auto c = 1; c < b.size() + 1; ++c) {
                if (a[r - 1] == b[c - 1]) { // No edits for current char needed
                    dp[r][c] = dp[r - 1][c - 1]; // Use whatever amount for prefix
                } else {
                    // Pick cheapest cost of either
                    // - deleting/skipping the mismatching char from a
                    // dp[r -1][c] is edits to reach the prefix ending at c but w/o the DELTE of the offending char"
                    const int if_delete = dp[r - 1][c] + 1;
                    // - adding the new char from b to some prefix
                    // dp[r][c - 1] is edits to reach the prefix ending at c - 1 so we need to ADD the offending char
                    const int if_add = dp[r][c - 1] + 1;
                    // - subbing the char
                    // dp[r - 1][c - 1] is edits to match the two prefixes ending at r - 1 and c - 1
                    const int if_sub = dp[r - 1][c - 1] + 1;
                    dp[r][c] = min3(if_delete, if_add, if_sub);
                }
            }
        }
        return dp[a.size()][b.size()];
    }
};