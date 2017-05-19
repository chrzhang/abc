#include <iostream>
#include <vector>
#include <cassert>
#include <climits>
#include <algorithm>
#include <fstream>
#include <sstream>

/*
From http://adventofcode.com/2015/day/24

--- Day 24: It Hangs in the Balance ---

It's Christmas Eve, and Santa is loading up the sleigh for this year's
deliveries. However, there's one small problem: he can't get the sleigh to
balance. If it isn't balanced, he can't defy physics, and nobody gets presents
this year.

No pressure.

Santa has provided you a list of the weights of every package he needs to fit
on the sleigh. The packages need to be split into three groups of exactly the
same weight, and every package has to fit. The first group goes in the
passenger compartment of the sleigh, and the second and third go in containers
on either side. Only when all three groups weigh exactly the same amount will
the sleigh be able to fly. Defying physics has rules, you know!

Of course, that's not the only problem. The first group - the one going in the
passenger compartment - needs as few packages as possible so that Santa has
some legroom left over. It doesn't matter how many packages are in either of
the other two groups, so long as all of the groups weigh the same.

Furthermore, Santa tells you, if there are multiple ways to arrange the
packages such that the fewest possible are in the first group, you need to
choose the way where the first group has the smallest quantum entanglement to
reduce the chance of any "complications". The quantum entanglement of a group
of packages is the product of their weights, that is, the value you get when
you multiply their weights together. Only consider quantum entanglement if the
first group has the fewest possible number of packages in it and all groups
weigh the same amount.

For example, suppose you have ten packages with weights 1 through 5 and 7
through 11. For this situation, some of the unique first groups, their quantum
entanglements, and a way to divide the remaining packages are as follows:

Group 1;             Group 2; Group 3
11 9       (QE= 99); 10 8 2;  7 5 4 3 1
10 9 1     (QE= 90); 11 7 2;  8 5 4 3
10 8 2     (QE=160); 11 9;    7 5 4 3 1
10 7 3     (QE=210); 11 9;    8 5 4 2 1
10 5 4 1   (QE=200); 11 9;    8 7 3 2
10 5 3 2   (QE=300); 11 9;    8 7 4 1
10 4 3 2 1 (QE=240); 11 9;    8 7 5
9 8 3      (QE=216); 11 7 2;  10 5 4 1
9 7 4      (QE=252); 11 8 1;  10 5 3 2
9 5 4 2    (QE=360); 11 8 1;  10 7 3
8 7 5      (QE=280); 11 9;    10 4 3 2 1
8 5 4 3    (QE=480); 11 9;    10 7 2 1
7 5 4 3 1  (QE=420); 11 9;    10 8 2

Of these, although 10 9 1 has the smallest quantum entanglement (90), the
configuration with only two packages, 11 9, in the passenger compartment gives
Santa the most legroom and wins. In this situation, the quantum entanglement
for the ideal configuration is therefore 99. Had there been two configurations
with only two packages in the first group, the one with the smaller quantum
entanglement would be chosen.

What is the quantum entanglement of the first group of packages in the ideal
configuration?

--- Part Two ---

That's weird... the sleigh still isn't balancing.

"Ho ho ho", Santa muses to himself. "I forgot the trunk".

Balance the sleigh again, but this time, separate the packages into four groups
instead of three. The other constraints still apply.

Given the example packages above, this would be some of the new unique first
groups, their quantum entanglements, and one way to divide the remaining
packages:

11 4    (QE=44); 10 5;   9 3 2 1; 8 7
10 5    (QE=50); 11 4;   9 3 2 1; 8 7
9 5 1   (QE=45); 11 4;   10 3 2;  8 7
9 4 2   (QE=72); 11 3 1; 10 5;    8 7
9 3 2 1 (QE=54); 11 4;   10 5;    8 7
8 7     (QE=56); 11 4;   10 5;    9 3 2 1

Of these, there are three arrangements that put the minimum (two) number of
packages in the first group: 11 4, 10 5, and 8 7. Of these, 11 4 has the lowest
quantum entanglement, and so it is selected.

Now, what is the quantum entanglement of the first group of packages in the
ideal configuration?
*/

int to_int(const std::string & s) {
    std::stringstream ss(s);
    int i = 0;
    if (!(ss >> i)) {
        std::cerr << "Could not read " << s << " as an int." << std::endl;
        assert(false);
    }
    return i;
}

class Solver {

    std::vector<unsigned long long> weights;
    unsigned long long target_weight;
    int number_groups;

    bool fits(const std::vector<bool> & bit_mask) const {
        assert(bit_mask.size() == weights.size());
        unsigned long long total_weight = 0;
        for (size_t i = 0; i < bit_mask.size(); ++i) {
            if (bit_mask[i]) {
                total_weight += weights[i];
            }
        }
        return total_weight == target_weight;
    }

    unsigned long long
    get_quantum_entanglement(const std::vector<bool> & bit_mask) const {
        assert(bit_mask.size() == weights.size());
        unsigned long long q_e = 1;
        for (size_t i = 0; i < bit_mask.size(); ++i) {
            if (bit_mask[i]) {
                q_e *= weights[i];
            }
        }
        return q_e;
    }

    bool group_weigh(const std::vector<unsigned long long> & remaining_weights,
                     std::vector<int> & groupings, size_t current_index) const {
        /* Recursively explore all ways of grouping the remaining gifts and
        find whether they can be cleanly put into groups that weigh the
        target weight. */
        if (current_index >= groupings.size()) { // Found grouping
            std::vector<unsigned long long>
                weights_in_groups(number_groups - 1, 0);
            for (size_t i = 0; i < groupings.size(); ++i) {
                const int & group = groupings[i];
                weights_in_groups[group] += remaining_weights[i];
            }
            for (const auto & weight : weights_in_groups) {
                if (weight != target_weight) {
                    return false;
                }
            }
            return true;
        }
        for (int i = 0; i < number_groups - 1; ++i) {
            groupings[current_index] = i;
            if (group_weigh(remaining_weights, groupings, current_index + 1)) {
                return true;
            }
        }
        return false;
    }

    bool can_fit_rest_of_gifts_aux(const std::vector<unsigned long long> &
                                   remaining_weights) const {
        std::vector<int> groupings(remaining_weights.size(), 0);
        return group_weigh(remaining_weights, groupings, 0);
    }

    bool can_fit_rest_of_gifts(const std::vector<bool> & bit_mask) const {
        std::vector<unsigned long long> remaining_gifts;
        for (size_t i = 0; i < bit_mask.size(); ++i) {
            if (!bit_mask[i]) {
                remaining_gifts.push_back(weights[i]);
            }
        }
        return can_fit_rest_of_gifts_aux(remaining_gifts);
    }

    void explore(std::vector<bool> & bit_mask, size_t capacity,
                 const size_t target, int current_index,
                 unsigned long long & q_e) {
        /* Recursively flip bits for all possibilities where `target` amount
           are flipped true in `bit_mask`. */
        if (capacity == target) {
            if (fits(bit_mask) && can_fit_rest_of_gifts(bit_mask)) {
                q_e = std::min(q_e, get_quantum_entanglement(bit_mask));
            }
            return;
        }
        if (current_index >= (int) bit_mask.size()) { return; }
        assert(target <= bit_mask.size());
        assert(capacity < target);
        bit_mask[current_index] = 1;
        explore(bit_mask, capacity + 1, target, current_index + 1, q_e);
        bit_mask[current_index] = 0;
        explore(bit_mask, capacity, target, current_index + 1, q_e);
    }

    public:
        Solver(const std::vector<unsigned long long> & weights,
               const int number_groups)
        : weights(weights), number_groups(number_groups) {
            int total_weight = 0;
            for (size_t i = 0; i < weights.size(); ++i) {
                total_weight += weights[i];
            }
            assert(total_weight % number_groups == 0);
            target_weight = total_weight / number_groups;
        }

        unsigned long long solve() {
            /*
            Since entertaining all validly weighed groupings consumes too
            much time, start by considering optimal cases and validating
            them as solutions starting from the most optimal candidate.
            The fewest number of items means entertaining all subsets starting
            from the smallest amount of things in the subset, then seeing if
            that set fulfills the restraints, and if it does, finding the most
            optimal subset among that size that has the lowest quantum
            entanglement.
            */
            std::vector<bool> bit_mask;
            for (size_t i = 0; i < weights.size(); ++i) {
                bit_mask.push_back(0);
            }
            for (size_t amount_of_items = 1; amount_of_items < weights.size();
                ++amount_of_items) {
                unsigned long long current_quantum_entanglement = ULLONG_MAX;
                // Quantum entanglement is used to compare between groupings
                // with the same amount of items
                explore(bit_mask, 0, amount_of_items, 0,
                    current_quantum_entanglement);
                if (current_quantum_entanglement < ULLONG_MAX) {
                    return current_quantum_entanglement;
                }
            }
            return ULLONG_MAX;
        }

};

int main(int argc, char * argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::ifstream f(argv[1]);
    if (!f.is_open()) {
        std::cerr << "Could not open " << argv[1] << std::endl;
        return 1;
    }
    std::string line;
    std::vector<unsigned long long> present_weights;
    while (std::getline(f, line)) {
        present_weights.push_back(to_int(line));
    }
    { // Part 1
        Solver s(present_weights, 3);
        assert(10723906903 == s.solve());
    }
    { // Part 2
        Solver s(present_weights, 4);
        assert(74850409 == s.solve());
    }
}
