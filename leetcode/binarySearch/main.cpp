#include <iostream>
#include <vector>
#include <cassert>

using namespace std;

int search_insert_aux(const vector<int> & nums, const int target,
                      const int begin, const int end) {
    if (begin > end) { return begin; }
    const int mid = (begin + end) / 2;
    if (nums[mid] < target) {
        return search_insert_aux(nums, target, mid + 1, end);
    } else if (nums[mid] > target) {
        return search_insert_aux(nums, target, begin, mid - 1);
    }
    return mid;
}

int search_insert(const vector<int> & nums, const int target) {
    if (nums.empty()) { return 0; }
    return search_insert_aux(nums, target, 0, (int) nums.size() - 1);
}

int main() {
    assert(1 == search_insert({1}, 2));
    assert(2 == search_insert({1, 3, 5, 6}, 5));
}
