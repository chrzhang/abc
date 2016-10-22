#include <iostream>
#include <vector>
#include <map>

// Find all distinct combinations that reach the desired sum

void findCombosAux(const std::map<int, size_t> & counts,
                   const std::map<int, size_t>::const_iterator & it,
                   int currSum,
                   int target,
                   std::map<int, size_t> curr_solution) {
    if (it == counts.end()) {
        if (currSum == target) {
            for (auto p : curr_solution) {
                std::cout << "\t>> Choose " << p.second << " of " << p.first
                          << "\n";
            }
            std::cout << "\t---------------------------------------\n";
        }
        return;
    }
    const auto & num = it->first;
    const auto & occ = it->second;
    // Pick between [0, occ] of num
    for (size_t amt = 0; amt <= occ; ++amt) {
        int thisSum = currSum + (amt * num);
        if (amt) {
            curr_solution[num] = amt;
        }
        findCombosAux(counts, std::next(it), thisSum, target, curr_solution);
    }
}

std::ostream & operator<<(std::ostream & os, const std::vector<int> & nums) {
    os << "{ ";
    for (auto n : nums) {
        os << n << " ";
    }
    os << "}";
    return os;
}

void findCombos(const std::vector<int> & nums,
                const int target) {
    std::cout << "Finding combinations of " << nums << " to reach "
              << target << std::endl;
    std::map<int, size_t> counts;
    std::map<int, size_t> curr_solution;
    for (auto n : nums) {
        counts[n] += 1;
    }
    findCombosAux(counts, counts.begin(), 0, target, curr_solution);
}

int main() {
    findCombos({10, 20, 30, 10}, 50);
    findCombos({1, 1, 1, 1, 1, 1}, 2);
    findCombos({2, 3, 6, 7}, 7);
    findCombos({1, 2, 3, 5}, 5);
    findCombos({-1, -2, -3, -5}, -5);
}
