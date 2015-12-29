#include <iostream>

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

int main() {
    for (int i = 0; true; ++i) {
        std::cout << "Ways to climb " << i << " steps: " << waysToClimb(i)
                  << std::endl;
    }
    return 0;
}
