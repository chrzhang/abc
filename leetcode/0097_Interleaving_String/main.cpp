class Solution {
public:
    bool isInterleaveAux(const string& s1, const string& s2, const string& s3,
        int s1i, int s2i, int s3i,
        map<vector<int>, bool>& results)
    {
        auto seek = results.find(vector<int>{ s1i, s2i, s3i });
        if (seek != results.end()) {
            return seek->second;
        }
        if (s1i >= s1.size() && s2i >= s2.size() && s3i >= s3.size()) {
            results[vector<int>{ s1i, s2i, s3i }] = true;
            return true;
        }
        if (s3i >= s3.size()) {
            return false;
        }
        if (s1i < s1.size()) {
            if ((s3[s3i] == s1[s1i]) && isInterleaveAux(s1, s2, s3, s1i + 1, s2i, s3i + 1, results)) {
                results[vector<int>{ s1i, s2i, s3i }] = true;
                return true;
            }
        }
        if (s2i < s2.size()) {
            if ((s3[s3i] == s2[s2i]) && isInterleaveAux(s1, s2, s3, s1i, s2i + 1, s3i + 1, results)) {
                results[vector<int>{ s1i, s2i, s3i }] = true;
                return true;
            }
        }
        results[vector<int>{ s1i, s2i, s3i }] = false;
        return false;
    }
    bool isInterleave(string s1, string s2, string s3)
    {
        if (s3.size() != s1.size() + s2.size()) {
            return false;
        }
        map<vector<int>, bool> results;
        return isInterleaveAux(s1, s2, s3, 0, 0, 0, results);
    }
};