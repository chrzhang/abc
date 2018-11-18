class Solution {
public:
    string getPermutation(const int N, const int K)
    {
        string s;
        for (int i = 1; i <= N; ++i) {
            s += i + '0';
        }
        int k = 1;
        do {
            if (k++ == K) {
                return s;
            }
        } while (next_permutation(s.begin(), s.end()));
        return s;
    }
};