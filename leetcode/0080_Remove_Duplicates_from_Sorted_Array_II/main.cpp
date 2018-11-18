class Solution {
public:
    int removeDuplicates(vector<int>& v)
    {
        int r(0), w(0), lv(0), lc(0);
        while (r < (int)v.size()) {
            if (w != r) {
                v[w] = v[r];
            }
            if (v[r] == lv && lc >= 2) {
                ++r;
            } else if (v[r] == lv) {
                ++lc;
                ++r;
                ++w;
            } else {
                lv = v[r];
                lc = 1;
                ++r;
                ++w;
            }
        }
        return w;
    }
};