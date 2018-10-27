class Solution {
public:
void caux(vector<vector<int>> & combos, const int index, const int sz, const int k,
          vector<int> & combo) {
    if (combo.size() == sz) {
        combos.push_back(combo);
        return;
    }
    if (index >= k) { return; }
    combo.push_back(index + 1);
    caux(combos, index + 1, sz, k, combo);
    combo.pop_back();
    caux(combos, index + 1, sz, k, combo);
}

vector<vector<int>> combine(int k, int sz) {
    vector<vector<int>> combos;
    if (sz < 0 || k < 0 || sz > k) { return combos; }
    vector<int> combo;
    caux(combos, 0, sz, k, combo);
    return combos;
}
};