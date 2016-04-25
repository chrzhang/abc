#include <iostream>
#include <vector>

// Finds path from top of a triangle to the base with the max possible sum

struct Triangle {
    std::vector<int> vec;
    Triangle(const std::vector<int> & v) : vec(v) {}
};

std::ostream & operator<<(std::ostream & os, const Triangle & t) {
    auto it = t.vec.begin();
    for (int level = 1; ; ++level) {
        for (int i = 0; i < level && it != t.vec.end(); ++i) {
            os << *it << " ";
            ++it;
        }
        if (it == t.vec.end()) { break; }
        os << "\n";
    }
    return os;
}

int main() {
    Triangle t(std::vector<int>({1, 2, 3, 4, 5, 6, 7, 8, 9, 10}));
    std::cout << t << std::endl;
    return 0;
}
