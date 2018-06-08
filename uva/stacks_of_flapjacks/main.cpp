#include <iostream>
#include <cassert>
#include <vector>
#include <algorithm>

using namespace std;

vector<int>::reverse_iterator findSpot(const int bigPancake,
                                       const vector<int>::reverse_iterator start,
                                       const vector<int>::reverse_iterator close) {
    // TODO Use binary search since [start, close] is sorted
    for (auto currIt = start; currIt <= close; ++currIt) {
        if (bigPancake > *currIt) return currIt;
    }
    return start;
}



void howToFlip(vector<int> pancakes, vector<int> & steps) {
    if (pancakes.size() <= 1) return;
    for (auto it = pancakes.rbegin(); next(it) != pancakes.rend(); ++it) {
        const int top = *next(it);
        const int bottom = *it;
        if (top > bottom) {
            auto pos = findSpot(top, pancakes.rbegin(), it);
            steps.push_back(1 + distance(pancakes.rbegin(), pos));
            reverse(pos, pancakes.rend());
            howToFlip(pancakes, steps);
            return;
        }
    }
}

vector<int> flip(vector<int> pancakes) {
    vector<int> steps = {};
    howToFlip(pancakes, steps);
    steps.push_back({0});
    return steps;
}

int main() {
    assert(vector<int>({1, 2, 0}) == flip({5, 1, 2, 3, 4}));
    assert(vector<int>({1, 0}) == flip({5, 4, 3, 2, 1}));
    assert(vector<int>({0}) == flip({1, 2, 3, 4, 5}));
    return 0;
}
