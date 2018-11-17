class Solution {
public:
    int fact(int n)
    {
        if (n < 0) {
            return -1;
        }
        if (n == 0) {
            return 1;
        } else {
            return n * fact(n - 1);
        }
    }

    int permutations(int n, int r)
    {
        return fact(n) / fact(n - r);
    }

    int countNumbersWithUniqueDigits(int n)
    {
        if (n < 0) {
            return 0;
        } else if (n == 0) {
            return 1;
        } else if (n == 1) {
            return 10;
        }
        return permutations(9, n) + (n - 1) * permutations(9, n - 1) + countNumbersWithUniqueDigits(n - 1);
    }
};