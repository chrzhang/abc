#include <iostream>
#include <assert.h>
#include <iomanip>

#define X 10
#define Y 10

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

size_t waysToReachDP(size_t (&table)[X + 1][Y + 1], const Position & src,
                     const Position & dest) {
    if (src.x == dest.x || src.y == dest.y) {
        table[src.x][src.y] = 1;
        return 1;
    }
    if (table[src.x][src.y]) { return table[src.x][src.y]; }
    size_t sum = waysToReachDP(table, Position(src.x + 1, src.y), dest) +
                 waysToReachDP(table, Position(src.x, src.y + 1), dest);
    table[src.x][src.y] = sum;
    return sum;
}

int main() {
    size_t table[X + 1][Y + 1]; // Store previous calculations
    for (int row = 0; row < X + 1; ++row) {
        for (int col = 0; col < Y + 1; ++col) {
            table[row][col] = 0;
        }
    }
    assert(waysToReachDP(table, Position(0,0), Position(X,Y)) ==
           waysToReach(Position(0,0), Position(X,Y)));
    for (int row = 0; row < X + 1; ++row) {
        for (int col = 0; col < Y + 1; ++col) {
            std::cout << std::setw(8) << table[row][col];
        }
        std::cout << std::endl;
    }
    return 0;
}
