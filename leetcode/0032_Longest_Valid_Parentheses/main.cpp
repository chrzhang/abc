class Solution {
public:
// Uses DP where each entry is the length of the longest valid substring ending
// at each index
// This restricts the length to 0 for all substrings ending in (
// For those ending in (), simply increasethe length of the valid substring
// before it by 2
// For those ending in )), check if there is a corresponding '(' for the last ')'
// and extend it by the newly considered substring and the one before it because
// two adjacent valid substrings form one valid substring
int longestValidParentheses(string s) {
    vector<int> dp(s.size(), 0);
    int result = 0;
    for (size_t i = 1; i < s.size(); ++i) {
        if (s[i] == '(') continue;
        if (s[i - 1] == '(') { // Found ()
            dp[i] = dp[i - 2] + 2;
        } else { // Found ))
            // Could have made something considered invalid before now valid
            // For finding the last ) in ()(()), the 2nd ( gets validated and
            // the valid substring directly before also extends the result
            if (s[i - dp[i - 1] - 1] == '(') {
                dp[i] = dp[i - 1] + dp[i - dp[i - 1] - 2] + 2;
            }
        }
        result = max(result, dp[i]);
    }
    return result;
}
};