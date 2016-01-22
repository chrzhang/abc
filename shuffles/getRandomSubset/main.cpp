#include <iostream>
#include <vector>
#include <cstdlib>
#include <ctime>
#include <unordered_map>
#include <string>
#include <cassert>

#define N 10
#define NUM_ITERATIONS 1000000

// Get a subset of a given size with fair probability

std::ostream & operator<<(std::ostream & os, const std::vector<int> & v) {
    for (auto it = v.begin(); it != v.end(); ++it) {
        os << *it << " ";
    }
    os << std::endl;
    return os;
}

void swap(int & a, int & b) {
    auto temp = a;
    a = b;
    b = temp;
}

std::vector<int> genSubset(int subsetSize, const std::vector<int> & v) {
    auto vCopy = v;
    for (int i = 0; i < subsetSize; ++i) {
        auto posToSwapWith = i + rand() % (N - i);
        swap(vCopy[i], vCopy[posToSwapWith]);
    }
    return std::vector<int>(vCopy.begin(),
                            std::next(vCopy.begin(), subsetSize));
}

std::string canonicalSet(const std::vector<int> & v) {
    std::string s;
    for (auto it = v.begin(); it != v.end(); ++it) {
        s += std::to_string(*it);
    }
    std::sort(s.begin(), s.end());
    return s;
}

unsigned choose(unsigned n, unsigned k) { // Refer to combinatronics equation
    if (k > n) { return 0; }
    unsigned r = 1;
    for (unsigned d = 1; d <= k; ++d) {
        r *= n--;
        r /= d;
    }
    return r;
}

int main() {
    srand(time(0));
    std::vector<int> v;
    v.reserve(N);
    for (int i = 0; i < N; ++i) {
        v.push_back(i);
    }
    std::cout << v;
    for (int subsetSize = 1; subsetSize <= N; ++subsetSize) {
        std::cout << "Sub-set size: " << subsetSize << std::endl;
        std::unordered_map<std::string, int> counts;
        for (int i = 0; i < NUM_ITERATIONS; ++i) {
            auto subset = genSubset(subsetSize, v);
            ++counts[canonicalSet(subset)];
        }
        double minPercentage = 100;
        double maxPercentage = 0;
        for (auto it = counts.begin(); it != counts.end(); ++it) {
            auto percentage = ((double) it->second) / NUM_ITERATIONS;
            percentage *= 100;
            if (percentage < minPercentage) { minPercentage = percentage; }
            if (percentage > maxPercentage) { maxPercentage = percentage; }
        }
        // Make sure every subset is made
        assert(choose(N, subsetSize) == counts.size());
        std::cout << "Each unordered subset occurs between " << minPercentage
                  << "% and " << maxPercentage << "%" << std::endl;
    }
    return 0;
}
