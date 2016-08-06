#include <iostream>
#include <set>
#include <utility>
#include <algorithm>
#include <cassert>

// Given two empty buckets with known capacities that can only be observed as
// full, empty, or in-between, find whether there is a way to obtain a total
// target amount of water between the two buckets by filling, emptying, or
// pouring one bucket into another

// Represent the state of the two buckets
struct BucketState {
    int amt1;
    int amt2;
    BucketState(int a1, int a2)
    : amt1(a1), amt2(a2) {
    }
    const int sum() const {
        return amt1 + amt2;
    }
};

bool operator<(const BucketState & stateA, const BucketState & stateB) {
    return std::tie(stateA.amt1, stateA.amt2) <
           std::tie(stateB.amt1, stateB.amt2);
}

bool exploreFromState(const BucketState currentState,
                      const int bucket1_capacity,
                      const int bucket2_capacity,
                      const int target,
                      std::set<BucketState> & exploredStates) {
    if (exploredStates.find(currentState) != exploredStates.end()) {
        return false;
    }
    exploredStates.insert(currentState);
    if (currentState.sum() == target) {
        return true;
    }
    // Fill the first bucket
    if (currentState.amt1 != bucket1_capacity) {
        if (exploreFromState(BucketState(bucket1_capacity, currentState.amt2),
                             bucket1_capacity, bucket2_capacity,
                             target, exploredStates)) {
            return true;
        }
    }
    // Fill the second bucket
    if (currentState.amt2 != bucket2_capacity) {
        if (exploreFromState(BucketState(currentState.amt1, bucket2_capacity),
                             bucket1_capacity, bucket2_capacity,
                             target, exploredStates)) {
            return true;
        }
    }
    // Empty the first bucket
    if (currentState.amt1 != 0) {
        if (exploreFromState(BucketState(0, currentState.amt2),
                             bucket1_capacity, bucket2_capacity,
                             target, exploredStates)) {
            return true;
        }
    }
    // Empty the second bucket
    if (currentState.amt2 != 0) {
        if (exploreFromState(BucketState(currentState.amt1, 0),
                             bucket1_capacity, bucket2_capacity,
                             target, exploredStates)) {
            return true;
        }
    }
    // Pour first into second bucket
    if (currentState.amt2 != bucket2_capacity &&
        currentState.amt1 > 0) {
        int leftover = currentState.amt1 + currentState.amt2 - bucket2_capacity;
        if (exploreFromState(BucketState(std::max(0, leftover),
                                         std::min(bucket2_capacity,
                                                  currentState.amt1 +
                                                  currentState.amt2)),
                             bucket1_capacity, bucket2_capacity,
                             target, exploredStates)) {
            return true;
        }
    }
    // Pour second into first bucket
    if (currentState.amt1 != bucket1_capacity &&
        currentState.amt2 > 0) {
        int leftover = currentState.amt1 + currentState.amt2 - bucket1_capacity;
        if (exploreFromState(BucketState(std::min(bucket1_capacity,
                                                  currentState.amt1 +
                                                  currentState.amt2),
                                         std::max(0, leftover)),
                             bucket1_capacity, bucket2_capacity,
                             target, exploredStates)) {
            return true;
        }
    }
    return false;
}

bool canContainExactly(const int bucket1_capacity,
                       const int bucket2_capacity,
                       const int target) {
    if (target == 0 || target == bucket1_capacity + bucket2_capacity) {
        return true;
    }
    std::set<BucketState> exploredStates;
    return exploreFromState(BucketState(0, 0),
                            bucket1_capacity,
                            bucket2_capacity,
                            target,
                            exploredStates);
}

int main() {
    assert(canContainExactly(3, 5, 4));
    assert(!canContainExactly(2, 6, 5));
    assert(!canContainExactly(2, 4, 3));
    assert(canContainExactly(2, 5, 1));
    assert(canContainExactly(9, 3, 6));
    assert(canContainExactly(3, 8, 7));
    assert(!canContainExactly(6, 1, 10));
    return 0;
}
