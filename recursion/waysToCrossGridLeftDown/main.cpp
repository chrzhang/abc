#include <iostream>
#include <assert.h>

#define X 3
#define Y 3

// Count ways from top left of a X + 1 by Y + 1 grid (0,0) to reach (X,Y)

struct Position {
    size_t x;
    size_t y;
    Position(size_t x, size_t y) {
        this->x = x;
        this->y = y;
    }
};

size_t waysToReach(const Position & src, const Position & dest) {
    if (src.x == dest.x || src.y == dest.y) {
        return 1;
    }
    return waysToReach(Position(src.x + 1, src.y), dest) +
           waysToReach(Position(src.x, src.y + 1), dest);
}

int main() {
    std::cout << waysToReach(Position(0,0), Position(X,Y)) << std::endl;
    assert(20 == waysToReach(Position(0,0), Position(3,3)));
    assert(10 == waysToReach(Position(0,0), Position(3,2)));
    return 0;
}
