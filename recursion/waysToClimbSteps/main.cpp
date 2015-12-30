#include <iostream>
#include <unordered_map>
#include <assert.h>

// Find how many ways are there to climb N steps (1,2,3 step increments)

size_t waysToClimb(size_t numSteps) {
    if (0 == numSteps) {
        return 1;
    }
    size_t acc = 0;
    if (numSteps >= 1) {
        acc += waysToClimb(numSteps - 1);
    }
    if (numSteps >= 2) {
        acc += waysToClimb(numSteps - 2);
    }
    if (numSteps >= 3) {
        acc += waysToClimb(numSteps - 3);
    }
    return acc;
}

size_t waysToClimbDP(std::unordered_map<size_t, size_t> & table,
                   size_t numSteps) {
    if (table.find(numSteps) != table.end()) {
        return table[numSteps];
    }
    if (0 == numSteps) {
        table[0] = 1;
        return 1;
    }
    size_t acc = 0;
    if (numSteps >= 1) {
        acc += waysToClimbDP(table, numSteps - 1);
    }
    if (numSteps >= 2) {
        acc += waysToClimbDP(table, numSteps - 2);
    }
    if (numSteps >= 3) {
        acc += waysToClimbDP(table, numSteps - 3);
    }
    table[numSteps] = acc;
    return acc;
}

int main() {
    std::unordered_map<size_t, size_t> table;
    for (int i = 0; true; ++i) {
        std::cout << "Ways to climb " << i << " steps: " << waysToClimb(i)
                  << std::endl;
        assert(waysToClimb(i) == waysToClimbDP(table, i));
    }
    return 0;
}
