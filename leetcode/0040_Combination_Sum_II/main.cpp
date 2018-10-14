class Solution {
public:
void solve(const vector<int> & cands, vector<bool> picked, const int curr_sum,
           const int i, const int target, set<multiset<int>> & result) {
    if (curr_sum == target) {
        multiset<int> t;
        for (size_t i = 0; i < picked.size(); ++i) {
            if (picked[i]) { t.insert(cands[i]); }
        }
        result.insert(t);
        return;
    }
    if (curr_sum > target  || i >= cands.size()) { return; }
    picked[i] = false;
    solve(cands, picked, curr_sum, i + 1, target, result);
    picked[i] = true;
    solve(cands, picked, curr_sum + cands[i], i + 1, target, result);
}

vector<vector<int>> combinationSum2(const vector<int>& candidates, int target) {
    set<multiset<int>> result;
    vector<bool> picked(candidates.size(), false);
    solve(candidates, picked, 0, 0, target, result);
    vector<vector<int>> ret;
    for (const auto & s : result) {
        vector<int> t(s.begin(), s.end());
        ret.push_back(t);
    }
    return ret;
}
};  