class Solution {
public:
    int maxArea(vector<int>& height)
    {
        int biggestVolSoFar = 0;
        int left(0), right(height.size() - 1);
        while (left < right) {
            biggestVolSoFar = max(biggestVolSoFar, min(height[left], height[right]) * (right - left));
            if (height[left] < height[right]) {
                ++left;
            } else {
                --right;
            }
        }
        return biggestVolSoFar;
    }
};