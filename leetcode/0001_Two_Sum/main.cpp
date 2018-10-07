class Solution {
public:
    vector<int> twoSum(vector<int>& nums, int target) {
        unordered_map<int, set<int>> numberToIndices;
        for (auto it = nums.begin(); it != nums.end(); ++it) {
            numberToIndices[*it].insert(it - nums.begin());
        }
        for (auto it = numberToIndices.begin(); it != numberToIndices.end(); ++it) {
            int number = it->first;
            auto seek = numberToIndices.find(target - number);
            if (seek == numberToIndices.end()) { continue; }
            auto i1 = it->second.begin();
            auto i2 = seek->second.begin();
            while (*i1 == *i2) {
                if (next(i1) != it->second.end()) { ++i1; }
                else if (next(i2) != seek->second.end()) { ++i2; }
                else {
                    break;
                }
            }
            if (*i1 == *i2) { continue; }
            if (*i1 < *i2) {
                return vector<int> {*i1 + 1, *i2 + 1};
            } else {
                return vector<int> {*i2 + 1, *i1 + 1};
            }
        }
    }
};
