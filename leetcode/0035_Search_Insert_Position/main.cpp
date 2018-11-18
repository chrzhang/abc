class Solution {
public:
    int searchInsertAux(vector<int>& nums, int target, int begin, int end)
    {
        if (begin > end) {
            return begin;
        }
        const int mid = (begin + end) / 2;
        if (nums[mid] < target)
            return searchInsertAux(nums, target, mid + 1, end);
        if (nums[mid] > target)
            return searchInsertAux(nums, target, begin, mid - 1);
        return mid;
    }
    int searchInsert(vector<int>& nums, int target)
    {
        return searchInsertAux(nums, target, 0, nums.size() - 1);
    }
};
