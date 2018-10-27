class Solution {
public:
    int findPeakElement(vector<int>& nums) {
        if (nums.size() == 1) { return 0; }
        for (auto it = nums.begin(); it != nums.end(); ++it) {
            // Check left
            auto leftVal = it == nums.begin() ? INT_MIN : *prev(it);
            auto rightVal = next(it) == nums.end() ? INT_MIN : *next(it);
            if (*it > leftVal && *it > rightVal) {
                return distance(nums.begin(), it);
            }
        }
    }
};