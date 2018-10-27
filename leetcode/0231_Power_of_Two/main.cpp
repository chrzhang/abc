class Solution {
public:
    bool isPowerOfTwo(int n) {
        if (n == 0) { return false; }
        // Has only 1 in its binary representation
        bool foundOnBit = 0;
        while (1) {
            if (n & 1) {
                if (foundOnBit) { return false; }
                foundOnBit = true;
            }
            n >>= 1;
            if (0 == n) {
                return true;
            }
        }
    }
};