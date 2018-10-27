class Solution {
public:
void incr(const int excl, int & i) {
    if (i + 1 == excl) { i += 2; }
    else { i++; }
}

void decr(const int excl, int & i) {
    if (i - 1 == excl) { i -= 2; }
    else { i--; }
}

void twoSumClosest(const vector<int> & nums, const int target,
                   const int excl, int & j, int & k) {
    int begin = excl == 0 ? 1 : 0;
    int end = excl == nums.size() - 1 ? nums.size() - 2 : nums.size() - 1;
    int closest_sum_diff = INT_MAX;
    while (begin < end) {
        const int diff = abs(target - (nums[begin] + nums[end]));
        if (diff < closest_sum_diff) {
            closest_sum_diff = diff;
            j = begin;
            k = end;
        }
        if (nums[begin] + nums[end] == target) {
            break;
        } else if (nums[begin] + nums[end] < target) {
            incr(excl, begin);
        } else {
            decr(excl, end);
        }
    }
}

int threeSumClosest(vector<int>& nums, int target) {
    sort(nums.begin(), nums.end());
    int closest_sum = 0;
    int closest_sum_diff = INT_MAX;
    for (int i = 0; i < nums.size(); ++i) {
        int j = -1;
        int k = -1;
        const int cmpl = target - nums[i];
        twoSumClosest(nums, cmpl, i, j, k);
        assert(j >= 0 && k >= 0);
        const int diff = abs(nums[i] + nums[j] + nums[k] - target);
        if (diff < closest_sum_diff) {
            closest_sum_diff = diff;
            closest_sum = nums[i] + nums[j] + nums[k];
        }
    }
    return closest_sum;
}

};