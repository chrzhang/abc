class Solution {
public:
    int find(const int begin, const int end, const vector<int>& v, const int target)
    {
        if (begin > end) {
            return -1;
        }
        const int mid = (begin + end) / 2;
        if (v[mid] == target) {
            return mid;
        }
        if (target < v[mid]) {
            return find(begin, mid - 1, v, target);
        } else {
            return find(mid + 1, end, v, target);
        }
    }
    vector<int> searchRange(const vector<int>& v, const int target)
    {
        const int found = find(0, v.size() - 1, v, target);
        vector<int> result;
        if (found < 0) {
            return { -1, -1 };
        }
        int i = found;
        while (i > 0 && v[i - 1] == target) {
            --i;
        }
        result.push_back(i);
        i = found;
        while (i < v.size() - 1 && v[i + 1] == target) {
            ++i;
        }
        result.push_back(i);
        return result;
    }
};