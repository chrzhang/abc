class Solution {
public:
    const string longestWCenter(const string & s, const size_t ii) {
        int offset = 0;
        while (ii - (offset + 1) >= 0 && ii + (offset + 1) < s.size() &&
               s[ii - (offset + 1)] == s[ii + (offset + 1)]) {
            ++offset;
        }
        return s.substr(ii - offset, 2 * offset + 1);
    }
    const string longestWCenterAfter(const string & s, const size_t ii) {
        int offset = 0;
        while (ii - (offset) >= 0 && ii + (offset + 1) < s.size() &&
               s[ii - (offset)] == s[ii + (offset + 1)]) {
            ++offset;
        }
        if (0 == offset) { return ""; }
        return s.substr(ii - (offset - 1), 2 * offset);
    }
    string longestPalindrome(string s) {
        string result = "";
        for (size_t ii = 0; ii < s.size(); ++ii) {
            const string l1 = longestWCenter(s, ii);
            if (l1.size() > result.size()) {
                result = l1;
            }
            const string l2 = longestWCenterAfter(s, ii);
            if (l2.size() > result.size()) {
                result = l2;
            }
        }
        return result;
    }
};