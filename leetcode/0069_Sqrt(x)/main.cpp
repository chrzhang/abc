class Solution {
public:
    int saux(const long begin, const long end, const long i)
    {
        if (begin > end) {
            if (begin * begin > i) {
                return end;
            }
            return begin;
        }
        const long mid = (begin + end) / 2;
        if (mid * mid == i) {
            return mid;
        }
        if (mid * mid < i) {
            return saux(mid + 1, end, i);
        }
        return saux(begin, mid - 1, i);
    }

    int mySqrt(const int i)
    {
        if (i < 2) {
            return i;
        }
        return saux(0, i, i);
    }
};