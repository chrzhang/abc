#include <iostream>
#include <vector>
#include <assert.h>

#define LENGTH 4

// Find all permutations

template<typename T>
std::vector<std::vector<T>> putAtEachPossibleLocation(T back,
                                const std::vector<T> & v) {
    std::vector<std::vector<T>> result;
    for (auto it = v.begin(); it != v.end(); ++it) {
        auto copyOfV = v;
        copyOfV.insert(copyOfV.begin() + (it - v.begin()), back);
        result.push_back(copyOfV);
    }
    auto copyOfV = v;
    copyOfV.insert(copyOfV.end(), back);
    result.push_back(copyOfV);
    return result;
}

template<typename T>
std::vector<std::vector<T>> genPermutations(std::vector<T> v) {
    std::vector<std::vector<T>> perms;
    if (v.empty()) {
        return perms;
    } else if (v.size() == 1) { // Base case
        perms.push_back(v);
        return perms;
    }
    T back = *v.rbegin();
    v.pop_back();
    auto subPerms = genPermutations(v);
    for (auto it = subPerms.begin(); it != subPerms.end(); ++it) {
        auto addition = putAtEachPossibleLocation(back, *it);
        perms.insert(perms.end(), addition.begin(), addition.end());
    }
    return perms;
}

template<typename T>
std::ostream & operator<<(std::ostream & os, const std::vector<T> & v) {
    os << "[";
    for (auto it = v.begin(); it != v.end(); ++it) {
        if (it != v.begin()) {
            os << ", ";
        }
        os << *it;
    }
    os << "]";
    return os;
}

int main() {
    for (int currentLength = 0; currentLength <= LENGTH; ++currentLength) {
        std::vector<int> v;
        for (int i = 1; i <= currentLength; ++i) {
            v.push_back(i);
        }
        std::cout << v << " has permutations: " << std::endl;
        auto perms = genPermutations(v);
        for (auto it = perms.begin(); it != perms.end(); ++it) {
            std::cout << "\t" << *it << std::endl;
        }
        std::cout << std::endl;
    }
    return 0;
}
