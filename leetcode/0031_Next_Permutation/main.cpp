class Solution {
public:
    void nextPermutation(vector<int>& nums) {
        // Start from end and go left in pairs, if the left number is less than the right, stop
        if (nums.size() < 2) { return; }
        auto left = next(nums.rbegin());
        auto right = nums.rbegin();
        bool found = false;
        while (left != nums.rend()) {
            if (*left < *right) {
                found = true;
                // Start from the right, and find the first number that is greater than *left
                for (auto it = nums.rbegin(); it != nums.rend(); ++it) {
                    if (*it > *left) {
                        swap(*it, *left);
                        break;
                    }
                }
                // Reverse the subvector from right to rbegin
                reverse(nums.rbegin(), left);
                break;
            }
            ++left;
            ++right;
        }
        // When the permutation is already maxed (e.g. 3, 2, 1)
        if (!found) { sort(nums.begin(), nums.end()); }
    }
};