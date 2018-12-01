class Solution {
public:
    bool saux(const vector<int>& nums, const int begin, const int end, const int target)
    {
        if (begin > end)
            return false;
        const int mid = (begin + end) / 2;
        if (nums[mid] == target)
            return true;
        if (mid > 0 && nums[begin] < nums[mid - 1] && target >= nums[begin] && target <= nums[mid - 1]) {
            return saux(nums, begin, mid - 1, target);
        }
        if (mid + 1 <= end && nums[mid + 1] < nums[end] && target >= nums[mid + 1] && target <= nums[end]) {
            return saux(nums, mid + 1, end, target);
        }
        return saux(nums, begin, mid - 1, target) || saux(nums, mid + 1, end, target);
    }
    bool search(const vector<int>& nums, const int target)
    {
        if (nums.empty()) {
            return false;
        }
        return saux(nums, 0, nums.size() - 1, target);
    }
};
