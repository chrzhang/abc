#include <iostream>
#include <cassert>
#include <vector>
#include <climits>
#include <fstream>
#include <algorithm>

using namespace std;

int sum(const vector<int> & team) {
    int total = 0;
    for (const auto & t : team) total += t;
    return total;
}
pair<int, int> solve(const vector<int> & weights) {
    const size_t total_weights = accumulate(weights.begin(), weights.end(), 0);
    // A 3D matrix indexed by [i][sum][j]
    vector<vector<vector<bool>>> dp(
        weights.size() + 1, // i, the end index of a range [0..i]
        vector<vector<bool>>(total_weights + 1, // sum, the total of a subset
            vector<bool>(weights.size() / 2 + 1, false))); // j, the # of elements in said subset
    // dp[i][sum][j] == true means we can fit j elements from the range [0..i] to make sum
    // If f[i-1][sum][j], so is f[i][sum][j]
    // If f[i][sum][j], f[i+1][sum + weights[i]][j + 1]
    for (size_t i = 0; i < weights.size() + 1; ++i) {
        for (size_t sum = 0; sum < total_weights + 1; ++sum) {
            for (size_t j = 0; j < weights.size() / 2 + 1; ++j) {
                dp[i][sum][j] = false;
            }
        }
    }
    for (size_t i = 0; i < weights.size() + 1; ++i) {
        for (size_t sum = 0; sum < total_weights + 1; ++sum) {
            for (size_t j = 0; j < weights.size() / 2 + 1; ++j) {
                if (sum == 0 && j == 0) {
                    dp[i][sum][j] = true;
                }
                if (i > 0 && dp[i - 1][sum][j]) {
                    dp[i][sum][j] = true;
                }
                if (dp[i][sum][j] && i + 1 < weights.size() + 1) {
                    dp[i + 1][sum + weights[i]][j + 1] = true;
                }
            }
        }
    }

    int best_diff = INT_MAX;
    int best_diff_a = 0;
    int best_diff_b = 0;
    // Look along j = weights.size() / 2
    for (size_t i = 0; i < weights.size() + 1; ++i) {
        for (size_t sum = 0; sum < total_weights + 1; ++sum) {
            if (dp[i][sum][weights.size() / 2]) {
                const size_t other_sum = total_weights - sum;
                if (abs((int) sum - (int) other_sum) < best_diff) {
                    best_diff = abs((int) sum - (int) other_sum);
                    best_diff_a = min(sum, other_sum);
                    best_diff_b = max(sum, other_sum);
                }
            }
        }
    }
    return make_pair(best_diff_a, best_diff_b);
}

void print_result(const pair<int, int> & result) {
    cout << result.first << " " << result.second << endl;
}

int main(int argc, const char * argv[]) {
    assert(make_pair(20, 68) == solve(vector<int>({68, 20})));
    assert(make_pair(100, 100) == solve(vector<int>({68, 50, 32, 30, 20})));
    assert(make_pair(190, 200) == solve(vector<int>({100, 90, 200})));
    assert(make_pair(243, 243) == solve(vector<int>({120, 111, 111, 101, 20, 11, 11, 1})));
    assert(make_pair(1879, 1880) == solve(vector<int>({142, 292, 446, 124, 171, 32, 430, 237, 4, 398, 217, 261, 354, 263, 59, 329})));
    if (2 != argc) {
        cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    ifstream inFile(argv[1]);
    int input_ct;
    inFile >> input_ct;
    for (int input_i = 0; input_i < input_ct; ++input_i) {
        int weight_ct;
        inFile >> weight_ct;
        vector<int> weights;
        for (int weight_i = 0; weight_i < weight_ct; ++weight_i) {
            int weight;
            inFile >> weight;
            weights.push_back(weight);
        }
        print_result(solve(weights));
        cout << endl;
    }
    return 0;
}
