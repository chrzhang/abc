class Solution {
vector<int> ints;
int t;
vector<vector<int>> result;
public:
    void solve(vector<int> counts, const int i, const int curr_sum) {
        if (curr_sum == t) {
            vector<int> r;
            for (int i = 0; i < counts.size(); ++i) {
                const int ct = counts[i];
                const int val = ints[i];
                vector<int> t(ct, val);
                r.insert(r.end(), t.begin(), t.end());
            }
            result.push_back(r);
            return;
        }
        if (curr_sum > t || i >= counts.size()) {
            return;
        }
        for (int a = 0; curr_sum + a * ints[i] <= t; ++a) {
            counts[i] = a;
            solve(counts, i + 1, curr_sum + a * ints[i]);
        }
    }
    vector<vector<int>> combinationSum(vector<int>& candidates, int target) {
        ints = candidates;
        t = target;
        vector<int> counts(candidates.size(), 0);
        solve(counts, 0, 0);
        return result;
    }
};