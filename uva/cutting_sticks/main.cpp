#include <iostream>
#include <vector>
#include <cassert>
#include <climits>
#include <list>
#include <fstream>

using namespace std;

int solve(const int length, vector<int> cut_indices) {
    cut_indices.insert(cut_indices.begin(), 0);
    cut_indices.push_back(length);
    const int cut_ct = cut_indices.size();
    // dp[i][j] = the min cost of making all cuts between i and j
    // If there are no cuts, the cost is 0.
    // If there are cuts, pick one to make first that will minimize cost.
    // Calculate what it would take to perform each cut, and pick the cheapest.
    // By building the matrix diagonal-by-diagonal, we have a direction to
    // generate values that will help us use DP and guarantee subranges of i
    // and j are already found.
    vector<vector<int>> dp(cut_ct, vector<int>(cut_ct, INT_MAX));
    // Since half the matrix is where i > j, we are wasting half the space.
    for (int start_c = 0; start_c < cut_ct; ++start_c) {
        int r, c; // row (i), column (j)
        r = 0;
        c = start_c;
        while (r < cut_ct && c < cut_ct) {
            if (0 == start_c || r + 1 == c) {
                dp[r][c] = 0;
            } else {
                int b_i = r + 1; // cut between i and j (b)
                assert(b_i < c);
                int min_cost = INT_MAX;
                while (b_i < c) {
                    assert(dp[r][b_i] != INT_MAX && dp[b_i][c] != INT_MAX);
                    // Cost of doing all cuts to the left and right of the
                    // current cut being considered.
                    const int curr_cost = dp[r][b_i] + dp[b_i][c];
                    min_cost = min(min_cost, curr_cost);
                    ++b_i;
                }
                dp[r][c] = cut_indices[c] - cut_indices[r] + min_cost;
            }
            ++r;
            ++c;
        }
    }
    return *dp[0].rbegin();
}

int main() {
    assert(20 == solve(10, {2, 4, 7}));
    assert(200 == solve(100, {25, 50, 75}));
    assert(22 == solve(10, {4, 5, 7, 8}));
    ifstream inFile("input.txt");
    int stick_length;
    while (inFile >> stick_length) {
        if (stick_length == 0) { return 0; }
        int cut_ct;
        inFile >> cut_ct;
        vector<int> cuts;
        for (int cut_i = 0; cut_i < cut_ct; ++cut_i) {
            int cut;
            inFile >> cut;
            cuts.push_back(cut);
        }
        cout << "The minimum cutting is " << solve(stick_length, cuts) << "."
             << endl;
    }
}
