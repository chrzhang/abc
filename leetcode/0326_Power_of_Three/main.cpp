class Solution {
public:
    bool isPowerOfThree(int n) {
        if (n <= 0) { return false; }
        int x = (int) (log10(n) / log10(3));
        int r = 1;
        for (int i = 0; i < x; ++i) {
            r *= 3;
        }
        return r == n;
    }
};