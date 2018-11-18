class Solution {
public:
    void part(vector<int>& v)
    {
        int b = 0;
        int e = v.size() - 1;
        while (b < e) {
            if (v[b] <= 0) {
                if (v[e] <= 0) { // v[b] neg, v[e] neg
                    ++b;
                } else { // v[b] neg, v[e] pos
                    ++b;
                    --e;
                }
            } else {
                if (v[e] <= 0) { // v[b] pos, v[e] neg
                    swap(v[b], v[e]);
                    ++b;
                    --e;
                } else { // v[b] pos, v[e] pos
                    --e;
                }
            }
        }
    }
    int first_pos(const vector<int>& v)
    {
        for (int i = 0; i < v.size(); ++i) {
            if (v[i] > 0)
                return i;
        }
        return -1;
    }
    int firstMissingPositive(vector<int> v)
    {
        part(v); // Partition into a negative side we ignore and a positive side
        const int fp = first_pos(v);
        if (fp == -1) {
            return 1;
        }
        // Iterate through positive ints, taking each value, using value as an index to mark values we found
        // We can mark without taking O(n) more space by negating existing value
        // Then iterate over a 3rd time, and find the first value that was not marked and hence was missing
        for (int i = fp; i < v.size(); ++i) {
            const int vi = abs(v[i]);
            if (fp + (vi - 1) >= v.size())
                continue;
            if (v[fp + (vi - 1)] > 0) {
                v[fp + (vi - 1)] *= -1;
            }
            assert(v[fp + (vi - 1)] < 0);
        }
        for (int i = fp; i < v.size(); ++i) {
            if (v[i] >= 0) {
                return 1 + (i - fp);
            }
        }
        return (v.size() - fp) + 1;
    }
};