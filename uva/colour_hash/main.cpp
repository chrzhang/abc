#include <iostream>
#include <cassert>
#include <array>
#include <set>

using namespace std;

bool solved(const array<int, 21> & board) {
    return board == array<int, 21>({0, 3, 4, 3,
                                    0, 5, 6, 5,
                                    0, 1, 2, 1,
                                    0, 7, 8, 7,
                                    0, 9, 10, 9,
                                    0});
}

void rotate(const array<int, 21> & order, array<int, 21> & board) {
    const array<int, 21> copy = board;
    for (int i = 0; i < 21; ++i) {
        board[i] = copy[order[i]];
    }
}

void rotate_right_wheel_right(array<int, 21> & board) {
    rotate(array<int, 21>({0, 1, 2, 3, 4, 5, 6, 7, 8,
                           11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 9, 10}), board);
}

void rotate_right_wheel_left(array<int, 21> & board) {
    rotate(array<int, 21>({0, 1, 2, 3, 4, 5, 6, 7, 8,
                           19, 20, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18}), board);
}

void rotate_left_wheel_right(array<int, 21> & board) {
    rotate(array<int, 21>({10, 11, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                           12, 13, 14, 15, 16, 17, 18, 19, 20}), board);
}

void rotate_left_wheel_left(array<int, 21> & board) {
    rotate(array<int, 21>({2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 0, 1,
                           12, 13, 14, 15, 16, 17, 18, 19, 20}), board);
}

void solve_aux(const array<int, 21> & board, string steps, string & min_steps,
               set<array<int, 21>> & visited) {
    if (steps.size() > min_steps.size()) return;
    visited.insert(board);
    if (solved(board)) {
        min_steps = min(steps, min_steps);
        return;
    }
    {
        array<int, 21> copy = board;
        rotate_right_wheel_right(copy);
        if (visited.find(copy) == visited.end()) {
            solve_aux(copy, steps + '2', min_steps, visited);
            visited.erase(copy);
        }
    }
    {
        array<int, 21> copy = board;
        rotate_right_wheel_left(copy);
        if (visited.find(copy) == visited.end()) {
            solve_aux(copy, steps + '4', min_steps, visited);
            visited.erase(copy);
        }
    }
    {
        array<int, 21> copy = board;
        rotate_left_wheel_right(copy);
        if (visited.find(copy) == visited.end()) {
            solve_aux(copy, steps + '1', min_steps, visited);
            visited.erase(copy);
        }
    }
    {
        array<int, 21> copy = board;
        rotate_left_wheel_left(copy);
        if (visited.find(copy) == visited.end()) {
            solve_aux(copy, steps + '3', min_steps, visited);
            visited.erase(copy);
        }
    }
}

string solve(const array<int, 21> & board) {
    string min_steps(17, '5');
    set<array<int, 21>> visited;
    solve_aux(board, "", min_steps, visited);
    if (min_steps.empty()) {
        return "PUZZLE ALREADY SOLVED";
    }
    if (min_steps.size() == 17) {
        return "NO SOLUTION WAS FOUND IN 16 STEPS";
    }
    return min_steps;
}

void test_rotate() {
    const array<int, 21> start({0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20});
    {
        auto board = start;
        rotate_left_wheel_left(board);
        const auto result = array<int, 21>({2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 0, 1, 12, 13, 14, 15, 16, 17, 18, 19, 20});
        assert(board == result);
        rotate_left_wheel_right(board);
        assert(board == start);
    }
    {
        auto board = start;
        rotate_right_wheel_left(board);
        const auto result = array<int, 21>({0, 1, 2, 3, 4, 5, 6, 7, 8, 19, 20, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18});
        assert(board == result);
        rotate_right_wheel_right(board);
        assert(board == start);
    }
}

int main() {
    test_rotate();
    assert("PUZZLE ALREADY SOLVED" == solve(array<int, 21>({0, 3, 4, 3,
                                                            0, 5, 6, 5,
                                                            0, 1, 2, 1,
                                                            0, 7, 8, 7,
                                                            0, 9, 10, 9,
                                                            0})));
    assert("1434332334332323" == solve(array<int, 21>({0, 3, 4, 5,
                                                       0, 3, 6, 5,
                                                       0, 1, 2, 1,
                                                       0, 7, 8, 7,
                                                       0, 9, 10, 9,
                                                       0})));
}
