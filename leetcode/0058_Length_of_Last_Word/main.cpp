class Solution {
public:
    int lengthOfLastWord(string s) {
        int r = 0;
        for (auto it = s.rbegin(); it != s.rend(); ++it) {
            if (*it != ' ') {
                for (auto it2 = it; it2 != s.rend(); ++it2) {
                    if (*it2 == ' ') {
                        break;
                    }
                    ++r;
                }
                return r;
            }
        }
        return r;
    }
};