class Solution {
    double paux(const double x, long n) {
        assert(n >= 0);
        if (n == 0) { return 1; }
        if (x == 1) { return x; }
        if (n % 2) { // Odd
            return paux(x, n - 1) * x;
        } else { // Even
            double r = paux(x, n / 2);
            return r * r;
        }
    }
public:
    double myPow(const double x, int n) {
        const double r = paux(x, labs(n));
        return n < 0 ? 1 / r : r;
    }
};
