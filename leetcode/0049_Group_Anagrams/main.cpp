class Solution {
public:
vector<vector<string>> groupAnagrams(const vector<string> & v) {
    map<string, vector<string>> d;
    for (const auto & w : v) {
        string s = w;
        sort(s.begin(), s.end());
        d[s].push_back(w);
    }
    vector<vector<string>> result;
    for (const auto & p : d) {
        vector<string> v;
        for (const auto & w : p.second) {
            v.push_back(w);
        }
        result.push_back(v);
    }
    return result;
}
};