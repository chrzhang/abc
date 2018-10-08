class Solution {
public:
    bool isPalindrome(int x) {
        int original = x;
        x = abs(x);
        int y = 0;
        while (x) {
            y *= 10;
            y += x % 10;
            x /= 10;
        }
        return y == original;
    }
};