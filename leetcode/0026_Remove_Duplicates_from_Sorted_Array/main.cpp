class Solution {
public:
    int removeDuplicates(vector<int>& nums)
    {
        if (nums.size() < 2) {
            return nums.size();
        }
        int r = 1;
        int w = 1;
        while (r < nums.size() && w < nums.size()) {
            while (r < nums.size() && nums[r] == nums[r - 1]) {
                ++r;
            }
            if (r >= nums.size())
                break;
            nums[w++] = nums[r++];
        }
        return w;
    }
};
