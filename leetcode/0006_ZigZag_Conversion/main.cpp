class Solution {
public:
    string convert(string s, int numRows)
    {
        if (numRows == 1) {
            return s;
        }
        int row = 0;
        int index = 0;
        int direction = 1; // 0 for UP, 1 for DOWN
        vector<string> allStrs;
        for (int i = 0; i < numRows; ++i) {
            allStrs.push_back("");
        }
        while (index < s.size()) {
            allStrs[row].push_back(s[index]);
            if (direction == 1) { // DOWN
                if (row == numRows - 1) {
                    row -= 1;
                    direction = 0;
                } else {
                    row += 1;
                }
            } else if (direction == 0) { // UP
                if (row == 0) {
                    row += 1;
                    direction = 1;
                } else {
                    row -= 1;
                }
            }
            ++index;
        }
        string result = "";
        for (const auto& s : allStrs) {
            result += s;
        }
        return result;
    }
};