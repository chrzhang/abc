#include <iostream>
#include <iomanip>
#include <set>
#include <cassert>
#include <ctime>
#include <cstdlib>
#include <vector>
#include <unordered_map>
#include <set>
#include <tuple>

#define N 5

// Find all a, b, c in the array that sum to 0

std::pair<size_t, size_t> genPair(size_t i1, size_t i2) {
    assert(i1 != i2);
    std::pair<size_t, size_t> r;
    r.first = i1 < i2 ? i1 : i2;
    r.second = i1 < i2 ? i2 : i1;
    return r;
}

void print(const std::vector<int> & v) {
    for (size_t i = 0; i < v.size(); ++i) {
        std::cout << "[" << i << "]: " << v[i] << " ";
    }
    std::cout << std::endl;
}

void print(const std::unordered_map<int, std::set<std::pair<size_t, size_t>>> &
           store) {
    for (auto p : store) {
        std::cout << std::setw(5) << p.first << ": ";
        for (auto q : p.second) {
            std::cout << "(" << q.first << "," << q.second << ") ";
        }
        std::cout << std::endl;
    }
}

int main() {
    srand(time(0));
    std::vector<int> v;
    for (int i = 0; i < N; ++i) {
        v.push_back(rand() % 11 - 5);
    }
    print(v);
    // Keep track of all pairs' sums
    std::unordered_map<int, std::set<std::pair<size_t, size_t>>> store;
    for (size_t i1 = 0; i1 < v.size(); ++i1) {
        for (size_t i2 = 0; i2 < v.size(); ++i2) {
            if (i1 == i2) { continue; }
            store[v[i1] + v[i2]].insert(genPair(i1, i2));
        }
    }
    std::set<std::tuple<size_t, size_t, size_t>> result;
    for (size_t i = 0; i < v.size(); ++i) {
        std::set<std::pair<size_t, size_t>> & s = store[-v[i]];
        if (!s.empty()) {
            // Go through set of pairs
            for (auto pairInSet : s) {
                if (pairInSet.first != i && pairInSet.second != i) {
                    if (i < pairInSet.first) {
                        result.insert(std::tuple<size_t, size_t, size_t>(i,
                                            pairInSet.first,
                                            pairInSet.second));
                    } else {
                        if (i < pairInSet.second) {
                            result.insert(std::tuple<size_t, size_t, size_t>(
                                                pairInSet.first, i,
                                                pairInSet.second));
                        } else {
                            result.insert(std::tuple<size_t, size_t, size_t>(
                                                pairInSet.first,
                                                pairInSet.second, i));
                        }
                    }
                }
            }
        }
    }
    for (auto triple : result) {
        std::cout << "[" << v[std::get<0>(triple)] << ","
                  << v[std::get<1>(triple)] << "," << v[std::get<2>(triple)]
                  << "]\n";
    }
    return 0;
}
