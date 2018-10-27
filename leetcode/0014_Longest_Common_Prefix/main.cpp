class Solution {
public:
    string longestCommonPrefix(vector<string>& strs) {
        std::string prefix;
        if (strs.empty()) { return prefix; }
        for (int currIndex = 0;;++currIndex) {
            if (currIndex >= strs[0].size()) { return prefix; }
            char c = strs[0][currIndex];
            for (int i = 1; i < strs.size(); ++i) {
                if (currIndex >= strs[i].size()) { return prefix; }
                if (strs[i][currIndex] != c) { return prefix; }
            }
            prefix.append(1, c);
        }
    }
};