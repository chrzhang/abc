#include <iostream>
#include <cassert>
#include <algorithm>
#include <unordered_map>

using namespace std;

int getLengthOfSeries(const int start) {
    static unordered_map<int, int> lengthsFound;
    auto got = lengthsFound.find(start);
    if (got != lengthsFound.end()) {
        return got->second;
    }
    if (1 == start) {
        return 1;
    }
    int next;
    if (start  % 2 == 1) { // Odd
        next = 3 * start + 1;
    } else {
        next = start / 2;
    }
    const int result = 1 + getLengthOfSeries(next);
    lengthsFound[start] = result;
    return result;
}

int getMaxLengthOfAllSeries(const int member1, const int member2) {
    const int low = min(member1, member2);
    const int high = max(member1, member2);
    int maxLength = -1;
    for (int i = low; i <= high; ++i) {
        maxLength = max(maxLength, getLengthOfSeries(i));
    }
    return maxLength;
}

int main() {
    assert(20 == getMaxLengthOfAllSeries(1, 10));
    assert(125 == getMaxLengthOfAllSeries(100, 200));
    assert(89 == getMaxLengthOfAllSeries(201, 210));
    assert(174 == getMaxLengthOfAllSeries(900, 1000));
}
