#include <iostream>
#include <cassert>
#include <vector>
#include <algorithm>
#include <fstream>
#include <sstream>
#include <stdexcept>

using namespace std;

void printVec(const vector<int> & v) {
    for (const auto i : v) {
        cout << i << " ";
    }
    cout << endl;
}

void flipAt(const size_t i, vector<int> & v) {
    assert(i < v.size());
    size_t end = i;
    for (size_t start = 0; start < end; ++start, --end) {
        swap(v[start], v[end]);
    }
}

const vector<int> getSorted(vector<int> v) {
    sort(v.begin(), v.end());
    return v;
}

vector<int> solve(vector<int> & pancakes) {
    vector<int> steps;
    const int numPancakes = pancakes.size();
    const vector<int> sortedPancakes = getSorted(pancakes);
    int indexOfOrderedStackStart = numPancakes;
    while (indexOfOrderedStackStart != 0) {
        int indexOfBiggestOutOfOrderPancake = -1;
        int max = -1;
        for (int i = 0; i < indexOfOrderedStackStart; ++i) {
            if (pancakes[i] > max) {
                max = pancakes[i];
                indexOfBiggestOutOfOrderPancake = i;
            }
        }
        if (indexOfBiggestOutOfOrderPancake != -1 &&
            indexOfBiggestOutOfOrderPancake != indexOfOrderedStackStart - 1) {
            // Since flipping the same stack twice nets nothing
            // Same goes for flipping at the top
            if (indexOfBiggestOutOfOrderPancake != 0) {
                flipAt(indexOfBiggestOutOfOrderPancake, pancakes);
                steps.push_back(indexOfBiggestOutOfOrderPancake);
            }
            if (indexOfOrderedStackStart - 1 != 0) {
                flipAt(indexOfOrderedStackStart - 1, pancakes);
                steps.push_back(indexOfOrderedStackStart - 1);
            }
        }
        bool foundNewUnordered = false;
        for (int i = numPancakes - 1; i >= 0; --i) {
            if (pancakes[i] != sortedPancakes[i]) {
                indexOfOrderedStackStart = i + 1;
                foundNewUnordered = true;
                break;
            }
        }
        if (!foundNewUnordered) {
            indexOfOrderedStackStart = 0;
        }
    }
    for (auto & step : steps) {
        step = numPancakes - step;
    }
    steps.push_back(0);
    return steps;
}

void test(vector<int> pancakes) {
    const vector<int> solution = getSorted(pancakes);
    solve(pancakes);
    assert(solution == pancakes);
}

int main() {
    test({1, 2, 3});
    test({1, 3, 2});
    test({2, 1, 3});
    test({2, 3, 1});
    test({3, 1, 2});
    test({3, 2, 1});
    vector<int> input_1({1, 2, 3, 4, 5});
    assert(vector<int>({0}) == solve(input_1));
    vector<int> input_2({5, 4, 3, 2, 1});
    assert(vector<int>({1, 0}) == solve(input_2));
    vector<int> input_3({5, 1, 2, 3, 4});
    assert(vector<int>({1, 2, 0}) == solve(input_3));
}
