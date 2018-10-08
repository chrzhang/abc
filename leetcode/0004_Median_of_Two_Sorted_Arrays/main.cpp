class Solution {
public:
    double findMedianSortedArrays(vector<int>& nums1, vector<int>& nums2) {
        auto totalSize = nums1.size() + nums2.size();
        if (totalSize == 0) { assert(false); }
        if (totalSize == 1) { return nums1.empty() ? nums2.front() : nums1.front(); }
        auto it1 = nums1.begin();
        auto it2 = nums2.begin();
        int target = totalSize / 2;
        int index = 0;
        int current, prev;
        current = prev = 0;
        for (;;) {
            ++index;
            if (it1 != nums1.end() && it2 != nums2.end()) {
                if (*it1 < *it2) {
                    prev = *it1;
                    ++it1;
                    if (it1 != nums1.end()) { current = *it1 < *it2 ? *it1 : *it2; }
                    else { current = *it2; }
                } else {
                    prev = *it2;
                    ++it2;
                    if (it2 != nums2.end()) { current = *it1 < *it2 ? *it1 : *it2; }
                    else { current = *it1; }
                }
            } else if (it1 != nums1.end()) {
                prev = *it1;
                ++it1;
                current = *it1;
            } else if (it2 != nums2.end()) {
                prev = *it2;
                ++it2;
                current = *it2;
            } else {
                assert(false);
            }
            if (index == target) {
                if (totalSize % 2 == 0) {
                    return (((double) prev) + current) / 2;
                } else {
                    return (double) (current);
                }
            }
        }
    }
};