class Solution {
public:
    int removeElement(vector<int>& nums, int val) {
        int r = 0;
        int w = 0;
        while (r < nums.size()) {
            while (nums[r] == val) { ++r; }
            if (r >= nums.size()) break;
            nums[w++] = nums[r++];
        }
        return w;
    }
};