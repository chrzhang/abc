class Solution {
public:
    vector<pair<int, int>> twoSum(const vector<int>& nums, const int excl, const int target)
    {
        vector<pair<int, int>> result;
        if (nums.size() < 2) {
            return result;
        }
        int begin = excl == 0 ? 1 : 0;
        int end = excl == nums.size() - 1 ? nums.size() - 2 : nums.size() - 1;
        while (begin < end) {
            if (nums[begin] + nums[end] == target) {
                result.push_back(make_pair(begin, end));
                if (begin + 1 == excl) {
                    ++begin;
                }
                ++begin;
                if (end - 1 == excl) {
                    --end;
                }
                --end;
            } else if (nums[begin] + nums[end] < target) {
                if (begin + 1 == excl) {
                    begin += 2;
                } else {
                    begin++;
                }
            } else {
                if (end - 1 == excl) {
                    end -= 2;
                } else {
                    end--;
                }
            }
        }
        return result;
    }

    vector<vector<int>> threeSum(vector<int> nums)
    {
        vector<vector<int>> result;
        if (nums.size() < 3) {
            return result;
        }
        sort(nums.begin(), nums.end());
        set<vector<int>> triplets;
        for (int i = 0; i < nums.size(); ++i) {
            // Find twoSum, excluding i
            const vector<pair<int, int>>& ts = twoSum(nums, i, -1 * nums[i]);
            for (const auto& p : ts) {
                if (p.first == -1)
                    continue;
                assert(nums[p.first] + nums[p.second] + nums[i] == 0);
                vector<int> v({ nums[i], nums[p.first], nums[p.second] });
                sort(v.begin(), v.end());
                triplets.insert(v);
            }
        }
        return vector<vector<int>>(triplets.begin(), triplets.end());
    }
};