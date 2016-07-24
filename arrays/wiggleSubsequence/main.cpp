#include <iostream>
#include <cassert>
#include <vector>
#include <algorithm>

// Given a sequence of numbers, find the length of the longest subsequence such
// that traversing the value alternates between increase and decrease

size_t wiggleMaxLength(const std::vector<int> & numbers) {
    if (numbers.size() < 2) {
        return numbers.size();
    }
    size_t count = 1;
    enum SEEKING { ANY, UP, DOWN };
    SEEKING target = ANY;
    for (size_t i = 1; i < numbers.size(); ++i) {
        if (numbers[i - 1] < numbers[i] &&
            (target == ANY || target == DOWN)) {
            ++count;
            target = UP;
        } else if (numbers[i - 1] > numbers[i]
                   && (target == ANY || target == UP)) {
            ++count;
            target = DOWN;
        }
    }
    return count;
}

void test() {
    assert(wiggleMaxLength({}) == 0);
    assert(wiggleMaxLength({1}) == 1);
    assert(wiggleMaxLength({1, 2}) == 2);
    assert(wiggleMaxLength({2, 1}) == 2);
    assert(wiggleMaxLength({0, 0}) == 1);
    assert(wiggleMaxLength({1, 7, 4, 9, 2, 5}) == 6);
    assert(wiggleMaxLength({1, 17, 5, 10, 13, 15, 10, 5, 16, 8}) == 7);
                         // ^  ^      ^   ^       ^      ^   ^
    assert(wiggleMaxLength({1, 2, 3, 4, 5, 6, 7, 8, 9}) == 2);
    assert(wiggleMaxLength({33, 53, 12, 64, 50, 41, 45, 21, 97, 35, 47}) == 10);
}

int main() {
    test();
    return 0;
}
