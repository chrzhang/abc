class Solution {
public:
    vector<vector<int>> generate(int numRows) {
        vector<vector<int>> result;
        for (int rowNum = 0; rowNum < numRows; ++rowNum) {
            vector<int> row;
            row.push_back(1);
            for (int k = 0; k < rowNum; ++k) {
                row.push_back(row.back() * (rowNum - k) / (double) (k + 1));
            }
            result.push_back(row);
        }
        return result;
    }
};