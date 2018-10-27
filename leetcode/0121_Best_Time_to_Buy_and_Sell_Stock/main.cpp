class Solution {
public:
    int maxProfit(vector<int>& prices) {
        if (prices.empty()) { return 0; }
        int currProfit = 0;
        int currMin = prices.front();
        for (auto p : prices) {
            if (p < currMin) {
                currMin = p;
            }
            if (p - currMin > currProfit) {
                currProfit = p - currMin;
            }
        }
        return currProfit;
    }
};