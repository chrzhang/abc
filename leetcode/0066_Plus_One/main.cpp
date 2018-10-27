class Solution {
public:
    vector<int> plusOne(vector<int>& v) {
        int carry = 1;
        int i = v.size() - 1;
        while (carry && i >= 0) {
            const int d = v[i];
            v[i] = (d + carry) % 10;
            carry = (d + carry) / 10;
            --i;
        }
        if (carry) {
            v.insert(v.begin(), carry);
        }
        return v;
    }
};
