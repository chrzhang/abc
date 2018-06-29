#include <iostream>
#include <vector>
#include <cassert>
#include <climits>
#include <algorithm>
#include <stdexcept>
#include <fstream>

using namespace std;

struct Value {
    size_t row;
    int v;
    Value(const size_t row, const int v)
    : row(row), v(v) {}
};

bool operator<(const Value & v1, const Value & v2) {
    return v1.v < v2.v;
}

vector<int> solve(const vector<vector<int>> & matrix) {
    if (matrix.empty() || matrix[0].empty()) {
        throw invalid_argument("Matrix is empty.");
    }
    auto path_weights = matrix;
    auto pred = matrix;
    for (size_t r = 0; r < path_weights.size(); ++r) {
        for (size_t c = 0; c < path_weights[r].size(); ++c) {
            pred[r][c] = -1;
            if (0 == c) continue;
            path_weights[r][c] = INT_MAX;
        }
    }
    for (size_t c = 1; c < path_weights[0].size(); ++c) {
        for (size_t r = 0; r < path_weights.size(); ++r) {
            vector<Value> left_3_neighbors;
            left_3_neighbors.push_back(Value(r, path_weights[r][c - 1]));
            if (r < path_weights.size() - 1) {
                left_3_neighbors.push_back(Value(r + 1, path_weights[r + 1][c - 1]));
            } else {
                left_3_neighbors.push_back(Value(0, path_weights[0][c - 1]));
            }
            if (r > 0) {
                left_3_neighbors.push_back(Value(r - 1, path_weights[r - 1][c - 1]));
            } else {
                left_3_neighbors.push_back(Value(path_weights.size() - 1,
                                                 path_weights[path_weights.size() - 1][c - 1]));
            }
            auto min_it = min_element(left_3_neighbors.begin(),
                                      left_3_neighbors.end());
            path_weights[r][c] = min_it->v + matrix[r][c];
            pred[r][c] = min_it->row;
        }
    }
    vector<int> result;
    int curr_col = matrix[0].size() - 1;
    result.push_back(matrix.size() - 1);
    int curr_pred = pred[matrix.size() - 1][curr_col];
    while (curr_pred != -1) {
        --curr_col;
        result.push_back(curr_pred);
        curr_pred = pred[curr_pred][curr_col];
    }
    return vector<int>(result.rbegin(), result.rend());
}

int main() {
    {
        auto result = solve({ {3, 4, 1, 2, 8, 6},
                              {6, 1, 8, 2, 7, 4},
                              {5, 9, 3, 9, 9, 5},
                              {8, 4, 1, 3, 2, 6},
                              {3, 7, 2, 8, 6, 4} });
        assert(result == vector<int>({0, 1, 2, 3, 3, 4}));
    }
    {
        auto result = solve({ {3, 4, 1, 2, 8, 6},
                              {6, 1, 8, 2, 7, 4},
                              {5, 9, 3, 9, 9, 5},
                              {8, 4, 1, 3, 2, 6},
                              {3, 7, 2, 1, 2, 3} });
        assert(result == vector<int>({0, 1, 0, 4, 4, 4}));
    }
    ifstream inFile("input.txt");
    int row_ct, col_ct;
    while (inFile >> row_ct && inFile >> col_ct) {
        vector<vector<int>> matrix;
        for (int row_i = 0; row_i < row_ct; ++row_i) {
            vector<int> row;
            for (int col_i = 0; col_i < col_ct; ++col_i) {
                int x;
                inFile >> x;
                row.push_back(x);
            }
            matrix.push_back(row);
        }
        auto result = solve(matrix);
        int sum = 0;
        int curr_col = 0;
        for (const auto & x : result) {
            cout << 1 + x << " ";
            sum += matrix[x][curr_col++];
        }
        cout << endl;
        cout << sum << endl;
    }
    return 0;
}
