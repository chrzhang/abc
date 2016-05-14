#include <iostream>
#include <cassert>
#include <set>

// S(i) is the sum of the squares of each digit in i
// T(x) is the sequence of S(i), S(S(i)), ...
// Find the smallest x such that T(x) contains a given parameter n

int squareDigits(int i) {
    auto s = std::to_string(i);
    int r = 0;
    for (auto c : s) {
        r += (c - '0') * (c - '0');
    }
    return r;
}

bool collectionOfSquares(int i, int target) {
    // Keep calling S(i) until it repeats or contains target
    int si = squareDigits(i);
    std::set<int> foundSoFar;
    auto seek = foundSoFar.find(si);
    while (seek == foundSoFar.end()) {
        foundSoFar.insert(si);
        if (si == target) { return true; }
        si = squareDigits(si);
        seek = foundSoFar.find(si);
    }
    return false;
}

int smallestResult(int n) {
    int x = 0;
    for (;;) {
        if (collectionOfSquares(x, n)) {
            return x;
        }
        ++x;
    }
}

int main() {
    assert(58 == squareDigits(37));
    assert(collectionOfSquares(37, 37)); // {58,89,145,42,20,4,16,37}
    assert(smallestResult(0) == 0);
    assert(smallestResult(2) == 11);
    assert(smallestResult(10) == 7);
    assert(smallestResult(1) == 1);
    assert(smallestResult(19) == 133);
    assert(smallestResult(85) == 5);
    assert(smallestResult(112) == 2666);
    return 0;
}
