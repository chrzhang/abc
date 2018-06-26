#include <iostream>
#include <cassert>
#include <array>
#include <map>
#include <list>
#include <set>

using namespace std;

static const array<char, 21> solution({0, 3, 4, 3,
                                       0, 5, 6, 5,
                                       0, 1, 2, 1,
                                       0, 7, 8, 7,
                                       0, 9, 10, 9,
                                       0});

bool solved(const array<char, 21> & board) {
    return board == solution;
}

void rotate(const array<char, 21> & order, array<char, 21> & board) {
    const array<char, 21> copy = board;
    for (int i = 0; i < 21; ++i) {
        board[i] = copy[order[i]];
    }
}

void rotate_right_wheel_right(array<char, 21> & board) {
    rotate(array<char, 21>({0, 1, 2, 3, 4, 5, 6, 7, 8,
                           11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 9, 10}), board);
}

void rotate_right_wheel_left(array<char, 21> & board) {
    rotate(array<char, 21>({0, 1, 2, 3, 4, 5, 6, 7, 8,
                           19, 20, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18}), board);
}

void rotate_left_wheel_right(array<char, 21> & board) {
    rotate(array<char, 21>({10, 11, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                           12, 13, 14, 15, 16, 17, 18, 19, 20}), board);
}

void rotate_left_wheel_left(array<char, 21> & board) {
    rotate(array<char, 21>({2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 0, 1,
                           12, 13, 14, 15, 16, 17, 18, 19, 20}), board);
}

void store_states(const string & steps, const array<char, 21> & board,
                  map<array<char, 21>, string> & states_to_steps) {
    static const int MIDDLE_BOUND = 8;
    if (steps.size() > MIDDLE_BOUND) return;
    auto seek = states_to_steps.find(board);
    if (seek != states_to_steps.end()) {
        if (steps.size() < seek->second.size() || steps < seek->second) {
            seek->second = steps;
        } else {
            return;
        }
    }
    states_to_steps[board] = steps;
    if (steps.size() >= MIDDLE_BOUND) return;
    {
        auto copy = board;
        rotate_left_wheel_right(copy);
        store_states(steps + '1', copy, states_to_steps);
    }
    {
        auto copy = board;
        rotate_right_wheel_right(copy);
        store_states(steps + '2', copy, states_to_steps);
    }
    {
        auto copy = board;
        rotate_left_wheel_left(copy);
        store_states(steps + '3', copy, states_to_steps);
    }
    {
        auto copy = board;
        rotate_right_wheel_left(copy);
        store_states(steps + '4', copy, states_to_steps);
    }
}

struct StateAndSteps {
    array<char, 21> state;
    string steps;
    StateAndSteps(const array<char, 21> state, const string & steps)
    : state(state), steps(steps)
    {}
};

string solve(const array<char, 21> & board) {
    // Entertaining all possible move sequences grows to 4 ^ 16 or 2 ^ 32.
    // The problem here is that 4^n gets out of hand very quickly.
    // 4 ^ 16 ~= 4 billion
    // Use a meet-in-the-middle approach by finding all states <= 8 turns away.
    // This process is bounded by 4 ^ 8 states.
    // Then do a breadth-first search from the target node, trying to see if
    // we can connect to an already found state. This is also 4 ^ 8 so overall
    // we have 2 * (4 ^ 8) or 2 ^ 17 or 131 thousand, which is much better.
    map<array<char, 21>, string> states_to_steps;
    store_states("", board, states_to_steps);
    list<StateAndSteps> queue;
    set<array<char, 21>> visited;
    queue.push_back(StateAndSteps(solution, ""));
    string result;
    bool link_found = false;
    while (!queue.empty()) {
        StateAndSteps current = queue.front();
        queue.pop_front();
        // TODO Might be multiple solutions, make sure to check
        auto seek = states_to_steps.find(current.state);
        if (seek != states_to_steps.end()) {
            result = seek->second + current.steps;
            // There may be multiple solutions, keep checking from the queue
            // for ones that would link AND get the min of those
            for (const auto & succ : queue) {
                if (succ.steps.size() > current.steps.size()) break;
                auto seek_alt = states_to_steps.find(succ.state);
                if (seek_alt != states_to_steps.end()) {
                    string alt_result = seek_alt->second + succ.steps;
                    result = min(result, alt_result);
                }
            }
            link_found = true;
            break;
        }
        if (current.steps.size() <= 9) {
            {
                auto copy = current.state;
                rotate_left_wheel_right(copy);
                if (visited.find(copy) == visited.end())
                    queue.push_back(StateAndSteps(copy, '3' + current.steps));
            }
            {
                auto copy = current.state;
                rotate_right_wheel_right(copy);
                if (visited.find(copy) == visited.end())
                    queue.push_back(StateAndSteps(copy, '4' + current.steps));
            }
            {
                auto copy = current.state;
                rotate_left_wheel_left(copy);
                if (visited.find(copy) == visited.end())
                    queue.push_back(StateAndSteps(copy, '1' + current.steps));
            }
            {
                auto copy = current.state;
                rotate_right_wheel_left(copy);
                if (visited.find(copy) == visited.end())
                    queue.push_back(StateAndSteps(copy, '2' + current.steps));
            }
        }
        visited.insert(current.state);
    }
    if (link_found && result.empty()) {
        return "PUZZLE ALREADY SOLVED";
    }
    if (result.empty() || result.size() > 16) {
        return "NO SOLUTION WAS FOUND IN 16 STEPS";
    }
    return result;
}

void test_rotate() {
    const array<char, 21> start({0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20});
    {
        auto board = start;
        rotate_left_wheel_left(board);
        const auto result = array<char, 21>({2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 0, 1, 12, 13, 14, 15, 16, 17, 18, 19, 20});
        assert(board == result);
        rotate_left_wheel_right(board);
        assert(board == start);
    }
    {
        auto board = start;
        rotate_right_wheel_left(board);
        const auto result = array<char, 21>({0, 1, 2, 3, 4, 5, 6, 7, 8, 19, 20, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18});
        assert(board == result);
        rotate_right_wheel_right(board);
        assert(board == start);
    }
}

int main() {
    test_rotate();
    assert("PUZZLE ALREADY SOLVED" == solve(array<char, 21>({0, 3, 4, 3,
                                                             0, 5, 6, 5,
                                                             0, 1, 2, 1,
                                                             0, 7, 8, 7,
                                                             0, 9, 10, 9,
                                                             0})));
    assert("1434332334332323" == solve(array<char, 21>({0, 3, 4, 5,
                                                        0, 3, 6, 5,
                                                        0, 1, 2, 1,
                                                        0, 7, 8, 7,
                                                        0, 9, 10, 9,
                                                        0})));
    assert("NO SOLUTION WAS FOUND IN 16 STEPS" == solve(array<char, 21>({0, 9, 4, 3,
                                                                         0, 5, 6, 5,
                                                                         0, 1, 2, 1,
                                                                         0, 7, 8, 7,
                                                                         0, 9, 10, 3,
                                                                         0})));
}
