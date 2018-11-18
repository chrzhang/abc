class Solution {
public:
    int candy(vector<int>& ratings)
    {
        vector<int> candy_cts(ratings.size(), 1);
        for (int i = 1; i < ratings.size(); ++i) {
            if (ratings[i] > ratings[i - 1]) {
                candy_cts[i] = candy_cts[i - 1] + 1;
            }
        }
        for (int i = ratings.size() - 2; i >= 0; --i) {
            if (ratings[i] > ratings[i + 1]) {
                candy_cts[i] = max(candy_cts[i], 1 + candy_cts[i + 1]);
            }
        }
        return accumulate(candy_cts.begin(), candy_cts.end(), 0);
    }
};