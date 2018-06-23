#include <iostream>
#include <cassert>
#include <vector>
#include <climits>
#include <fstream>

using namespace std;

int sum(const vector<int> & team) {
    int total = 0;
    for (const auto & t : team) total += t;
    return total;
}

pair<int, int> solve(const vector<int> & weights) {
    if (weights.size() < 2) { throw invalid_argument("Not enough people."); }
    vector<int> team_A, team_B;
    auto mid_iter = next(weights.begin(), weights.size() / 2);
    team_A = vector<int>(weights.begin(), mid_iter);
    team_B = vector<int>(mid_iter, weights.end());
    int team_A_sum = sum(team_A);
    int team_B_sum = sum(team_B);
    int curr_sum_diff = abs(team_A_sum - team_B_sum);
    for (auto & team_A_member : team_A) { // Consider swapping each member
        // Find best candidate in team B to swap with
        int smallest_pot_diff = curr_sum_diff;
        int smallest_pot_diff_A_sum = 0;
        int smallest_pot_diff_B_sum = 0;
        int * swap_from_a = nullptr;
        int * swap_from_b = nullptr;
        for (auto & team_B_member : team_B) {
            const int pot_A_sum = team_A_sum - team_A_member + team_B_member;
            const int pot_B_sum = team_B_sum - team_B_member + team_A_member;
            const int pot_new_diff = abs(pot_A_sum - pot_B_sum);
            if (pot_new_diff < smallest_pot_diff) {
                smallest_pot_diff = pot_new_diff;
                smallest_pot_diff_A_sum = pot_A_sum;
                smallest_pot_diff_B_sum = pot_B_sum;
                swap_from_a = &team_A_member;
                swap_from_b = &team_B_member;
            }
        }
        if (smallest_pot_diff < curr_sum_diff) {
            curr_sum_diff = smallest_pot_diff;
            team_A_sum = smallest_pot_diff_A_sum;
            team_B_sum = smallest_pot_diff_B_sum;
            swap(*swap_from_a, *swap_from_b);
        }
    }
    return make_pair(min(team_A_sum, team_B_sum),
                     max(team_A_sum, team_B_sum));
}

void print_result(const pair<int, int> & result) {
    cout << result.first << " " << result.second << endl;
}

int main() {
    assert(make_pair(20, 68) == solve(vector<int>({68, 20})));
    assert(make_pair(100, 100) == solve(vector<int>({68, 50, 32, 30, 20})));
    assert(make_pair(190, 200) == solve(vector<int>({100, 90, 200})));
    ifstream inFile("input.txt");
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
    }
    return 0;
}
