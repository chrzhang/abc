class Solution {
public:
void incr(int & i, const int & excl1, const int & excl2) {
    i ++;
    while (i == excl1 || i == excl2) {
        i++;
    }
}
void decr(int & i, const int & excl1, const int & excl2) {
    i--;
    while (i == excl1 || i == excl2) {
        i--;
    }
}

vector<pair<int, int>> twoSum(const vector<int> & nums, const int target, const int excl1, const int excl2) {
    vector<pair<int, int>> result;
    int begin = -1;
    int end = nums.size();
    incr(begin, excl1, excl2);
    decr(end, excl1, excl2);
    while (begin < end) {
        if (nums[begin] + nums[end] == target) {
            result.push_back(make_pair(begin, end));
            incr(begin, excl1, excl2);
            decr(end, excl1, excl2);
        } else if (nums[begin] + nums[end] < target) {
            incr(begin, excl1, excl2);
        } else {
            decr(end, excl1, excl2);
        }
    }
    return result;
}

vector<vector<int>> fourSum(vector<int>& nums, int target) {
    sort(nums.begin(), nums.end());
    set<vector<int>> result;
    for (int i = 0; i < nums.size(); ++i) {
        for (int j = i + 1; j < nums.size(); ++j) {
            const vector<pair<int, int>> vp = twoSum(nums, target - (nums[i] + nums[j]), i, j);
            for (const auto & p :  vp) {
                vector<int> v({nums[i], nums[j], nums[p.first], nums[p.second]});
                sort(v.begin(), v.end());
                result.insert(v);
            }
        }
    }
    return vector<vector<int>>(result.begin(), result.end());
}
};