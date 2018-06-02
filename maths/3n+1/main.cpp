#include <iostream>
#include <cassert>
#include <algorithm>

using namespace std;

int getLengthOfSeries(const int start) {
    int current = start;
    int length = 0;
    do {
        ++length;
        if (1 == current) {
            break;
        } else if (current % 2 == 1) {
            current = 3 * current + 1;
        } else {
            current = current / 2;
        }
    } while (true);
    return length;
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
