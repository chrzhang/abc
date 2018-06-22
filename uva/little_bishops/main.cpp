#include <vector>
#include <iostream>
#include <cassert>
#include <set>

using namespace std;

bool in_bounds(const int x, const int N) {
    return x >= 0 && x < N;
}

bool safe(const int r, const int c, const vector<vector<bool>> & occupied) {
    const int N = occupied.size();
    static const vector<int> deltas = {-1, 1};
    for (const auto & row_delta : deltas) {
        for (const auto & col_delta : deltas) {
            int new_r = r;
            int new_c = c;
            while (in_bounds(new_r, N) && in_bounds(new_c, N)) {
                if (occupied[new_r][new_c]) return false;
                new_r += row_delta;
                new_c += col_delta;
            }
        }
    }
    return true;
}

void ways_aux(const int bishop_count_remaining,
              vector<vector<bool>> & occupied,
              set<vector<vector<bool>>> & configs,
              const bool white) {
    if (bishop_count_remaining == 0) {
        configs.insert(occupied);
        return;
    }
    for (size_t r = 0; r < occupied.size(); ++r) {
        size_t start_c = 0;
        if (white) {
            start_c = (r % 2 == 1 ? 0 : 1);
        } else {
            start_c = (r % 2 == 1 ? 1 : 0);
        }
        for (size_t c = start_c; c < occupied.size(); c += 2) {
            // For every <white|black> cell
            if (safe(r, c, occupied)) {
                occupied[r][c] = true;
                ways_aux(bishop_count_remaining - 1,
                         occupied,
                         configs,
                         white);
                occupied[r][c] = false;
            }
        }
    }
}

int ways(const int bishop_count, const int board_size, const bool white) {
    set<vector<vector<bool>>> configs;
    vector<vector<bool>> occupied(board_size, vector<bool>(board_size, false));
    ways_aux(bishop_count, occupied, configs, white);
    return configs.size();
}

int solve(const int bishop_count,
          const int board_size) {
    int total = 0;
    for (int i = 0; i <= bishop_count; ++i) {
        const int bcw = i;
        const int bcb = bishop_count - bcw;
        total += ways(bcw, board_size, true) * ways(bcb, board_size, false);
    }
    return total;
}

int main() {
    assert(5599888 == solve(6, 8));
    assert(260 == solve(4, 4));
}
