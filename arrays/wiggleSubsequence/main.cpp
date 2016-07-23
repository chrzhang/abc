#include <iostream>
#include <cassert>
#include <vector>
#include <algorithm>

// Given a sequence of numbers, find the length of the longest subsequence such
// that traversing the value alternates between increase and decrease

// Add the number to the subsequence if it would result in a series of
// alternating increases and decreases
int addIfWiggle(std::vector<int> & subsequence, int num) {
    if (subsequence.size() < 2) {
        subsequence.push_back(num);
        return 0;
    }
    auto rit = subsequence.rbegin();
    if ((*rit > *std::next(rit) && num < *rit) ||
        (*rit < *std::next(rit) && num > *rit)) {
        subsequence.push_back(num);
        return 0;
    }
    return 1;
}

// Helper recursive function that explores all possibilities of including or
// not including a number in the subsequence.
void wiggleMaxLengthAux(const std::vector<int> & numbers, size_t currentIndex,
                        size_t & max, std::vector<int> currSubsequence) {
    if (currentIndex >= numbers.size()) {
        max = std::max(max, currSubsequence.size());
        return;
    }
    // Ignore the current index
    wiggleMaxLengthAux(numbers, currentIndex + 1, max, currSubsequence);
    // Include the current index
    if (0 == addIfWiggle(currSubsequence, numbers[currentIndex])) {
        wiggleMaxLengthAux(numbers, currentIndex + 1, max, currSubsequence);
    }
}


size_t wiggleMaxLength(const std::vector<int> & numbers) {
    size_t max = 0;
    wiggleMaxLengthAux(numbers, 0, max, std::vector<int>());
    return max;
}

void test() {
    assert(wiggleMaxLength({}) == 0);
    assert(wiggleMaxLength({1}) == 1);
    assert(wiggleMaxLength({1, 2}) == 2);
    assert(wiggleMaxLength({2, 1}) == 2);
    assert(wiggleMaxLength({1, 7, 4, 9, 2, 5}) == 6);
    assert(wiggleMaxLength({1, 17, 5, 10, 13, 15, 10, 5, 16, 8}) == 7);
                         // ^  ^      ^   ^       ^      ^   ^
    assert(wiggleMaxLength({1, 2, 3, 4, 5, 6, 7, 8, 9}) == 2);
}

int main() {
    test();
    return 0;
}
