class Solution {
public:
    int divide(int dividend, int divisor) {
        long int result = 0;
        const bool is_neg = (dividend < 0) != (divisor < 0); // XOR is !=
        long dend = labs((long) dividend);
        long dsor = labs((long) divisor);
        for (;;) {
            long int ds = dsor;
            long int x = 1;
            while (dend >= ds) {
                ds <<= 1;
                x <<= 1;
            }
            if (x != 1) {
                x >>= 1;
                ds >>= 1;
                dend -= ds;
                result += x;
            } else {
                break;
            }
        }
        while (dend > 0) {
            if (dend - dsor >= 0) ++result;
            dend -= dsor;
        }
        if (is_neg) result *= -1;
        if (result > INT_MAX || result < INT_MIN) { return INT_MAX; }
        return result;
    }
};