class Solution {
public:
    bool isMatch(const string& t, const string& p)
    {
        const size_t PSZ = p.size();
        const size_t TSZ = t.size();
        // Make a DP table for whether each prefix of pattern and text match
        // dp[i][j] = whether text[0, i] would match pattern[0, j]
        vector<vector<bool>> dp(TSZ + 1, vector<bool>(PSZ + 1, false));
        dp[0][0] = true; // Empty string with empty regex
        for (size_t i = 1; i <= PSZ && p[i - 1] == '*'; ++i) {
            dp[0][i] = true; // If the pattern begins with *s
        }
        for (size_t ti = 1; ti <= TSZ; ++ti) {
            for (size_t pi = 1; pi <= PSZ; ++pi) {
                if (t[ti - 1] == p[pi - 1] || p[pi - 1] == '?') { // Current char matches, so matches if rest of prefix matched
                    dp[ti][pi] = dp[ti - 1][pi - 1];
                } else if (p[pi - 1] == '*') {
                    // * can mean empty string, so matches if we can ignore the star and there's a match
                    // * can also mean any sequence, so matches if we consider the current character matched and check rest of prefix
                    dp[ti][pi] = dp[ti][pi - 1] || dp[ti - 1][pi];
                }
            }
        }
        return dp[TSZ][PSZ];
    }
};