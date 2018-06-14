#include <iostream>
#include <cassert>
#include <array>
#include <list>

using namespace std;
using wheels_t = array<char, 4>;

wheels_t spin(wheels_t ws, const int i, const bool clockwise) {
    assert(i >= 0 && i <= 3);
    if (clockwise) {
        ws[i] = ws[i] == 0 ? 9 : ws[i] - 1;
    } else {
        ws[i] = ws[i] == 9 ? 0 : ws[i] + 1;
    }
    return ws;
}

int toInt(const wheels_t & ws) {
    return 1000 * ws[0] + 100 * ws[1] + 10 * ws[2] + ws[3];
}

array<wheels_t, 8> get_neighbors(const wheels_t & ws) {
    array<wheels_t, 8> result;
    for (int i = 0; i < 8; ++i) {
        result[i] = spin(ws, i / 2, i % 2 == 0);
    }
    return result;
}

bool contains(const list<wheels_t> & wss, const wheels_t & wheels) {
    for (const auto & ws : wss) {
        if (ws == wheels) return true;
    }
    return false;
}

int solve(const wheels_t & start,
          const wheels_t & target,
          const list<wheels_t> & forbidden) {
    if (start == target) return 0;
    assert(!contains(forbidden, target));
    bool found[9999 + 1] = { false };
    for (const auto & ws : forbidden) {
        found[toInt(ws)] = true;
    }
    found[toInt(start)] = true;
    list<pair<wheels_t, int>> traversal_queue = { make_pair(start, 0) };
    while (!traversal_queue.empty()) {
        const wheels_t & curr_ws = traversal_queue.front().first;
        const int curr_step = traversal_queue.front().second;
        if (curr_ws == target) {
            return curr_step;
        }
        const auto & neighbors = get_neighbors(curr_ws);
        for (const auto & neighbor : neighbors) {
            const int neighbor_int = toInt(neighbor);
            if (!found[neighbor_int]) {
                found[neighbor_int] = true;
                traversal_queue.push_back(make_pair(neighbor, curr_step + 1));
            }
        }
        traversal_queue.pop_front();
    }

    return -1;
}

int main() {
    assert(toInt({1, 2, 3, 4}) == 1234);
    assert(14 == solve({8, 0, 5, 6}, {6, 5, 0, 8},
                       { {8, 0, 5, 7},
                         {8, 0, 4, 7},
                         {5, 5, 0, 8},
                         {7, 5, 0, 8},
                         {6, 4, 0, 8} }));
    assert(-1 == solve({0, 0, 0, 0}, {5, 3, 1, 7},
                       { {0, 0, 0, 1},
                         {0, 0, 0, 9},
                         {0, 0, 1, 0},
                         {0, 0, 9, 0},
                         {0, 1, 0, 0},
                         {0, 9, 0, 0},
                         {1, 0, 0, 0},
                         {9, 0, 0, 0} }));
}
