#include <iostream>
#include <cassert>
#include <algorithm>

// Find the combined area of two rectangles

// The lower left of the first rectangle is at (A,B), the upper right at (C,D)
// The lower left of the other rectangle is at (E,F), the upper right at (G,H)
int computeArea(int A, int B, int C, int D, int E, int F, int G, int H) {
    // Add the areas of the rectangles and subtract the overlap
    int a1 = (C - A) * (D - B);
    int a2 = (G - E) * (H - F);
    int sum = a1 + a2;
    if (a1 && a2 && !((G <= A) || (E >= C) || (F >= D) || (H <= B))) {
        sum -= (std::min(C, G) - std::max(A, E)) *
               (std::min(H, D) - std::max(F, B));
    }
    return sum;
}

int main() {
    assert(computeArea(-3, 0, 3, 4, 0, -1, 9, 2)  == 6 * 4 + 9 * 3 - 6);
    assert(computeArea(0, -1, 9, 2, -3, 0, 3, 4)  == 6 * 4 + 9 * 3 - 6);
    assert(computeArea(0, 0, 0, 0, 0, -1, 9, 2)  == 9 * 3);
    assert(computeArea(-1, -1, 1, 1, -1, -1, 1, 1)  == 2 * 2);
    return 0;
}
