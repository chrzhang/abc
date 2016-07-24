#include <iostream>
#include <cassert>
#include <vector>
#include <algorithm>

// Given a sequence of numbers, find the length of the longest subsequence such
// that traversing the value alternates between increase and decrease

/*
All sequences of length smaller than 2 are trivially wiggly. For the general
case, a number under consideration is Current, with a Previous and Next number.
In terms of ordering 3 numbers, there are 3! possibilites of greatest to least
ordering with the case of equal ordering able to be generalized into the
ignored cases below.

C - Current
N - Next
P - Previous

6 Possible orderings (Higher is greater)

1. C can be safely ignored without affecting wiggly max length
P
 \
  C
   \
    N

2. C is included to calculate wiggly max length
P
 \   N
  \ /
   C

3. C is included to calculate wiggly max length
   C
  / \
 /   N
P

4. C is included to calculate wiggly max length
  C
 / \
P   \
     N

5. C can be safely ignored without affecting wiggly max length
    N
   /
  C
 /
P

6. C is included to calculate wiggly max length
     N
P   /
 \ /
  C
*/
size_t wiggleMaxLength(const std::vector<int> & numbers) {
    if (numbers.size() < 2) {
        return numbers.size();
    }
    size_t count = 1;
    enum SEEKING { ANY, UP, DOWN };
    SEEKING target = ANY;
    // Start at second element so we have a Previous, Current, and Next number
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
        // Otherwise, safely ignore
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
