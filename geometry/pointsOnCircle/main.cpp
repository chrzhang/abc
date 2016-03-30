#include <iostream>
#include <cmath>

// Given a unit circle of radius r, find the number of [x,y] points on the
// circle where x and y are integers

bool isSquareRootOf(int root, double n) {
    return root * root == n;
}

int numIntCoordPoints(double radius) {
    int numPoints = 0;
    // Need to only handle one quadrant and multiply by 4
    for (double x = 1; x < radius; ++x) {
        double ySquared = radius * radius - x * x;
        if (isSquareRootOf((int) std::sqrt(ySquared), ySquared)) {
            ++numPoints;
        }
    }
    numPoints *= 4;
    // Handle edge cases of being on axis separately
    double intPart;
    if (std::modf(radius, &intPart) == 0) {
        numPoints += 4;
    }
    return numPoints;
}

void printStatsForRadius(int r) {
    std::cout << "Radius: " << r << " has " << numIntCoordPoints(r)
              << " border points.\n";
}

int main() {
    printStatsForRadius(1);
    printStatsForRadius(2);
    printStatsForRadius(5);
    return 0;
}
