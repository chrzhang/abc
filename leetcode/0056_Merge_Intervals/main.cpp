class Solution {
public:
    vector<Interval> merge(vector<Interval>& intervals)
    {
        vector<Interval> result;
        sort(intervals.begin(), intervals.end(), [](const Interval& a, const Interval& b) { return a.start < b.start; });
        if (intervals.empty()) {
            return result;
        }
        Interval ci = intervals[0];
        for (size_t i = 1; i < intervals.size(); ++i) {
            const Interval& ni = intervals[i];
            if (ni.start <= ci.end) {
                ci.end = max(ci.end, ni.end);
            } else {
                result.push_back(ci);
                ci = ni;
            }
        }
        result.push_back(ci);
        return result;
    }
};