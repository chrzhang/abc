class Solution {
public:
    void sortColors(vector<int>& v)
    { // Dutch National Flag problem
        int lo(0), mid(0), hi((int)v.size() - 1);
        // [0..lo-1] is RED
        // [lo..mid-1] is WHITE
        // [hi+1..N-1] is BLUE
        // [mid..hi] is UNKNOWN
        while (mid <= hi) {
            // At every step, shrink the UNKNOWN REGION by ++mid or --hi
            if (v[mid] == 0) {
                swap(v[mid], v[lo]);
                ++lo; // Grow RED
                ++mid; // v[mid] got a v[lo] which is WHITE
            } else if (v[mid] == 1) {
                ++mid; // Grow WHITE
            } else if (v[mid] == 2) {
                swap(v[mid], v[hi]);
                --hi; // Grow BLUE
            } else {
                throw runtime_error("Wrong val: " + to_string(mid));
            }
        }
    }
};