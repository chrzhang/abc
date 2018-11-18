class Solution {
public:
    int maxSubArray(const vector<int>& v)
    {
        vector<int> result;
        vector<int> s(v.size(), 0);
        vector<int> b(v.size(), 0);
        int max_s = v[0];
        s[0] = v[0];
        for (size_t i = 1; i < v.size(); ++i) {
            if (v[i] + s[i - 1] > v[i]) {
                s[i] = v[i] + s[i - 1];
                b[i] = b[i - 1];
            } else {
                s[i] = v[i];
                b[i] = i;
            }
            max_s = max(max_s, s[i]);
        }
        return max_s;
    }
};