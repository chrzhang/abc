class Solution {
    vector<int> wh;
    void left_to_right(const vector<int>& height)
    {
        for (size_t i = 0; i < height.size(); ++i) {
            if (wh[i] != 0) {
                continue;
            }
            for (size_t e = i + 1; e < height.size(); ++e) {
                if (height[e] >= height[i]) {
                    for (size_t m = i + 1; m < e; ++m) {
                        wh[m] = min(height[i], height[e]) - height[m];
                    }
                    break;
                }
            }
        }
    }
    void right_to_left(const vector<int>& height)
    {
        for (int i = height.size() - 1; i >= 0; --i) {
            if (wh[i] != 0) {
                continue;
            }
            for (int e = i - 1; e >= 0; --e) {
                if (height[e] >= height[i]) {
                    for (int m = i - 1; m > e; --m) {
                        wh[m] = min(height[i], height[e]) - height[m];
                    }
                    break;
                }
            }
        }
    }

public:
    int trap(vector<int>& height)
    {
        wh = vector<int>(height.size(), 0); // water heights
        left_to_right(height);
        right_to_left(height);
        return accumulate(wh.begin(), wh.end(), 0);
    }
};