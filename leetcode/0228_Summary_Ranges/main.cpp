class Solution {
public:
    vector<string> summaryRanges(vector<int>& nums) {
        vector<string> result;
        for (auto it = nums.begin(); it != nums.end();) {
            auto b = *it;
            auto walker = next(it);
            if (nums.end() == walker) {
                result.push_back(to_string(b));
                return result;
            }
            while (*walker == *prev(walker) + 1) {
                ++walker;
            }
            walker = prev(walker);
            if (b != *walker) {
                result.push_back(to_string(b) + "->" + to_string(*walker));
            } else {
                result.push_back(to_string(b));
            }
            it = next(walker);
        }
        return result;
        
    }
};