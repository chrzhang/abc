class Solution {
public:
    int majorityElement(vector<int>& nums)
    {
        unordered_map<int, int> counts;
        for (auto n : nums) {
            auto seek = counts.find(n);
            if (seek == counts.end()) {
                counts[n] = 1;
                if (counts[n] > nums.size() / 2) {
                    return n;
                }
            } else {
                ++(seek->second);
                if (seek->second > nums.size() / 2) {
                    return seek->first;
                }
            }
        }
    }
};
