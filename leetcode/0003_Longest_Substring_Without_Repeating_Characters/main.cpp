class Solution {
public:
    int lengthOfLongestSubstring(string s)
    {
        int longestLengthSoFar = 0;
        for (size_t ii = 0; ii < s.size(); ++ii) {
            bool charsUsed[128] = { 0 };
            charsUsed[s[ii]] = true;
            int currentLength = 1;
            for (size_t jj = ii + 1; jj < s.size(); ++jj) {
                if (charsUsed[s[jj]]) {
                    break;
                }
                charsUsed[s[jj]] = true;
                ++currentLength;
            }
            longestLengthSoFar = max(longestLengthSoFar, currentLength);
        }
        return longestLengthSoFar;
    }
};