class Solution {
public:
void saux(const vector<int> & nums, const int i,
          vector<vector<int>> & result, vector<int> & currss) {
    if (i >= nums.size()) {
        result.push_back(currss);
    } else {
        currss.push_back(nums[i]);
        saux(nums, i + 1, result, currss);
        currss.pop_back();
        saux(nums, i + 1, result, currss);
    }
}
vector<vector<int>> subsets(const vector<int>& nums) {
    vector<vector<int>> result;
    vector<int> subset;
    saux(nums, 0, result, subset);
    return result;
}
};