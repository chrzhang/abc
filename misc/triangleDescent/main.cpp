#include <iostream>
#include <vector>
#include <climits>
#include <cassert>

// Finds path from top of a triangle to the base with the max possible sum

struct Triangle {
    std::vector<std::pair<int, int>> vec;
    Triangle(const std::vector<int> & v) {
        auto it = v.begin();
        for (int level = 1; it != v.end(); ++level) {
            for (int i = 0; i < level && it != v.end(); ++i) {
                vec.push_back(std::make_pair(level, *it));
                ++it;
            }
        }
    }
    bool outOfBounds(int i) const {
        return i < 0 || i >= (int) vec.size();
    }
    int getLeftChildIndex(int i) const {
        if (outOfBounds(i)) { return -1; }
        const std::pair<int, int> & p = vec[i];
        if (outOfBounds(i + p.first)) { return -1; }
        return i + p.first;
    }
    int getRightChildIndex(int i) const {
        if (outOfBounds(i)) { return -1; }
        const std::pair<int, int> & p = vec[i];
        if (outOfBounds(i + p.first + 1)) { return -1; }
        return i + p.first + 1;
    }
    int maxSumPath() const {
        std::vector<int> table;
        int max = INT_MIN;
        for (size_t i = 0; i < vec.size(); ++i) {
            auto plpi = i - (vec[i].first - 1) - 1;
            auto prpi = i - (vec[i].first - 1);
            int lpi =
                (outOfBounds(plpi) || vec[plpi].first != vec[i].first - 1)
                ? -1 : plpi;
            int rpi =
                (outOfBounds(prpi) || vec[prpi].first != vec[i].first - 1)
                ? -1 : prpi;
            if (lpi != -1 && rpi != -1) {
                table.push_back(std::max(vec[i].second + table[lpi],
                                         vec[i].second + table[rpi]));
            } else if (lpi != -1) {
                table.push_back(vec[i].second + table[lpi]);
            } else if (rpi != -1) {
                table.push_back(vec[i].second + table[rpi]);
            } else {
                table.push_back(vec[i].second);
            }
            if (vec[i].first == vec.back().first) { // On last level
                max = std::max(max, table.back());
            }
        }
        // Look at the last row of the dynamic programming table triangle
        return max;
    }
};

std::ostream & operator<<(std::ostream & os, const std::pair<int, int> & p) {
    if (p.first == -1) { os << "-"; }
    else { os << "(" /*<< p.first << ", "*/ << p.second << ")"; }
    return os;
}

std::ostream & operator<<(std::ostream & os, const Triangle & t) {
    int i = 0;
    for (auto p : t.vec) {
        os << p;
        int lci = t.getLeftChildIndex(i);
        int rci = t.getRightChildIndex(i);
        std::pair<int, int> dummy(-1, -1);
        os << "\tLeft child: " << ((lci == -1) ? dummy : t.vec[lci])
           << "\tRight child: " << ((lci == -1) ? dummy : t.vec[rci])
           << std::endl;
        ++i;
    }
    return os;
}

int main() {
    {
        Triangle t(std::vector<int>({1, 2, 3, 4, 5, 6, 7, 8, 9, 10}));
        assert(20 == t.maxSumPath()); // 1 + 3 + 6 + 10
    }
    {
        Triangle t(std::vector<int>({1, 6, 7, 4, -1, 6, 5, 8, 9, 0}));
        assert(23 == t.maxSumPath()); // 1 + 7 + 6 + 9
    }
    {
        Triangle t(std::vector<int>({5, 4, 1, 2, 8, -1}));
        assert(17 == t.maxSumPath()); // 5 + 4 + 8
    }
    return 0;
}
