#include <iostream>
#include <vector>

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
    int getLeftChildIndex(int i) const {
        if (i < 0 || i >= (int) vec.size()) { return -1; }
        const std::pair<int, int> & p = vec[i];
        if (i + p.first >= (int) vec.size()) { return -1; }
        return i + p.first;
    }
    int getRightChildIndex(int i) const {
        if (i < 0 || i >= (int) vec.size()) { return -1; }
        const std::pair<int, int> & p = vec[i];
        if (i + p.first + 1 >= (int) vec.size()) { return -1; }
        return i + p.first + 1;
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
    Triangle t(std::vector<int>({1, 2, 3, 4, 5, 6, 7, 8, 9, 10}));
    std::cout << t << std::endl;
    return 0;
}
