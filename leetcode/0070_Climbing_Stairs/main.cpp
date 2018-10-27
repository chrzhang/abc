class Solution {
public:
int climbStairs(int N) {
    N += 1;
    if (N < 3) { return 1; }
    vector<int> dp(N, 1);
    for (int i = 2; i < N; ++i) {
        dp[i] = dp[i - 1] + dp[i - 2];
    }
    return dp[N - 1];
}
};