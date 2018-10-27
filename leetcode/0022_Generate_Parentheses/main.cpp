class Solution {
public:
    void gaux(const int no, const int nc, const string & s, vector<string> & result) {
        if (no + nc == 0) {
            result.push_back(s);
            return;
        }
        const int bal = nc - no;
        assert(bal >= 0);
        if (nc && bal > 0) {
            gaux(no, nc - 1, s + ")", result);
        }
        if (no) {
            gaux(no - 1, nc, s + "(", result);
        }
    }
    vector<string> generateParenthesis(int n) {
        vector<string> result;
        gaux(n, n, "", result);
        return result;
    }
};