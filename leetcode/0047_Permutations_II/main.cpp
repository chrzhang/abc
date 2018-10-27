class Solution {
public:
    vector<vector<int>> permuteUnique(vector<int>& nums) {
        set<vector<int>> result;
        sort(nums.begin(), nums.end());
        do {
            result.insert(nums);
        } while (next_permutation(nums.begin(), nums.end()));
        return vector<vector<int>>(result.begin(), result.end());
    }
};