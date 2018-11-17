class Solution {
public:
    string countAndSay(const int n)
    {
        if (n == 1) {
            return "1";
        }
        string result = countAndSay(n - 1);
        int curr_count = 0;
        char curr_dig = 0;
        string ret;
        for (size_t i = 0; i <= result.size(); ++i) {
            char c;
            if (i == result.size()) {
                c = 0;
            } else {
                c = result[i];
            }
            if (curr_count) {
                if (c != curr_dig) {
                    ret += to_string(curr_count) + string(1, curr_dig);
                    curr_count = 1;
                    curr_dig = c;
                } else {
                    ++curr_count;
                }
            } else {
                curr_count = 1;
                curr_dig = c;
            }
        }
        return ret;
    }
};