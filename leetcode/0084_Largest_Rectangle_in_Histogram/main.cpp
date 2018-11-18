class Solution {
public:
    int largestRectangleArea(vector<int> heights)
    { // https://stackoverflow.com/a/35931960
        int result = 0;
        // Add sentinel values to generalize edge cases
        // We consider an area for our solution when we pop a bar, so we must pop every bar to consider every area
        heights.push_back(-1); // So eventually every bar gets popped (in the case the histogram ends in heights 1, 2, 3 for example)
        heights.insert(heights.begin(), -1); // So the stack never goes empty because no height can be < 0
        list<int> stack = { 0 }; // The stack starts never empty and will never become empty
        for (int i = 1; i < (int)heights.size(); ++i) {
            while (heights[i] < heights[stack.back()]) {
                const int popped = stack.back();
                stack.pop_back();
                const int h = heights[popped];
                // stack.back() must be less than the popped value (or equal, but then eventually stack.back() will be less than popped)
                // So what was popped marks a block (not necessarily the leftmost) of a possible rectangle of height h
                // Because the stack can only ever be increasing, everything to the right between the next i and stack.back() gives the width
                // If stack.back() is not exactly 1 before popped, we can still use this equation because the chunk that was in between must have
                // been > popped and been includable as part of the rectangle
                result = max(result, h * (i - stack.back() - 1));
            }
            stack.push_back(i);
        }
        return result;
    }
};
