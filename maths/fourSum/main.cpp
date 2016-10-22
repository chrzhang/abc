#include <iostream>
#include <vector>
#include <unordered_map>
#include <set>
#include <algorithm>
#include <array>

// Find all a, b, c, d that sum to 0

bool nothing_shared_between(const std::pair<int, int> & p1,
                            const std::pair<int, int> & p2) {
    return p1.first != p2.first && p1.first != p2.second &&
           p1.second != p2.first && p1.second != p2.second;
}

void display(const std::vector<int> & vec) {
    std::cout << "Data >> ";
    for (auto x : vec) {
        std::cout << x << " ";
    }
    std::cout << "\n";
}

std::set<std::array<int, 4>> fourSum(const std::vector<int> & vec,
                                     const int target) {
    display(vec);
    std::unordered_map<int, std::set<std::pair<int, int>>> sum_to_pairs;
    for (auto x = vec.begin(); x != vec.end(); ++x) {
        for (auto y = std::next(x); y != vec.end(); ++y) {
            sum_to_pairs[*x + *y].insert({std::distance(vec.begin(), x),
                                          std::distance(vec.begin(), y)});
        }
    }
    std::set<std::array<int, 4>> results;
    for (auto entry : sum_to_pairs) {
        auto & curr_sum = entry.first;
        auto & curr_pairs = entry.second;
        auto seek = sum_to_pairs.find(target - curr_sum);
        if (seek != sum_to_pairs.end()) {
            // Make sure no values are re-used
            auto & inv_pairs = seek->second;
            for (auto curr_pair : curr_pairs) {
                for (auto inv_pair : inv_pairs) {
                    if (nothing_shared_between(curr_pair, inv_pair)) {
                            std::array<int, 4> values =
                                { { vec[curr_pair.first],
                                    vec[curr_pair.second],
                                    vec[inv_pair.first],
                                    vec[inv_pair.second] } };
                            std::sort(values.begin(), values.end());
                            results.insert(values);
                    }
                }
            }
        }
    }
    for (auto r : results) {
        std::cout << r[0] << " " << r[1] << " " << r[2] << " " << r[3] << "\n";
    }
    return results;
}

int main() {
    fourSum({2, 2, 3, 4, 5, -1, -8}, 0);
    fourSum({1,0,-1,0,-2,2}, 0);
    return 0;
}
