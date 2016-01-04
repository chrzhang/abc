#include <iostream>
#include <set>
#include <assert.h>

#define SIZE 8
#define N 8

// Print all ways n queens can be arranged peacefully on a chessboard

struct Position {
    size_t x, y;
    Position(size_t x, size_t y) : x(x), y(y) {}
};

struct PositionCompare {
    bool operator() (const Position & p1, const Position & p2) const {
        if (p1.x != p2.x) {
            return p1.x < p2.x;
        }
        return p1.y < p2.y;
    }
};

struct EncodingCompare {
    bool operator() (const std::bitset<SIZE * SIZE> & b1,
                     const std::bitset<SIZE * SIZE> & b2) const {
        for (int i = 0; i < SIZE * SIZE; ++i) {
            if (b1[i] - '0' == b2[i] - '0') {
                continue;
            }
            return (b1[i] - '0') < (b2[i] - '0');
        }
        return false;
    }
};

std::bitset<SIZE * SIZE> encodeQueenPos(
    const std::set<Position, PositionCompare> & queenSpots) {
    std::bitset<SIZE * SIZE> encoding;
    for (int row = 0; row < SIZE; ++row) {
        for (int col = 0; col < SIZE; ++col) {
            if (queenSpots.find(Position(row, col)) != queenSpots.end()) {
                encoding[row * SIZE + col] = '1';
            }
        }
    }
    return encoding;
}

std::ostream & operator<<(std::ostream & os, const std::bitset<SIZE * SIZE> e) {
    for (int i = 0; i < SIZE * SIZE; ++i) {
        os << e[i];
    }
    os << std::endl;
    return os;
}

void printBoard(const std::set<Position, PositionCompare> & safeSpots,
                const std::set<Position, PositionCompare> & queenSpots) {
    for (int row = 0; row < SIZE; ++row) {
        for (int col = 0; col < SIZE; ++col) {
            if (safeSpots.find(Position(row, col)) == safeSpots.end()) {
                if (queenSpots.find(Position(row, col)) != queenSpots.end()) {
                    std::cout << "Q ";
                } else {
                    std::cout << "X ";
                }
            } else {
                std::cout << "_ ";
            }
        }
        std::cout << std::endl;
    }
}

void printBoard(const std::set<Position, PositionCompare> & queenSpots) {
    for (int row = 0; row < SIZE; ++row) {
        for (int col = 0; col < SIZE; ++col) {
            if (queenSpots.find(Position(row, col)) != queenSpots.end()) {
                std::cout << "Q ";
            } else {
                std::cout << "_ ";
            }
        }
        std::cout << std::endl;
    }
}

bool queenAttack(const Position & src, const Position & target) {
    if (target.x == src.x) { return true; }
    if (target.y == src.y) { return true; }
    if ((target.x > src.x ? target.x - src.x : src.x - target.x) ==
        (target.y > src.y ? target.y - src.y : src.y - target.y)) {
        return true;
    }
    return false;
}

// Modify the set of safe positions with an attack strategy (can be generalized
// for other pieces)
void updateSafeSpots(const Position & p,
                     bool (*canAttack)(const Position & src,
                                       const Position & target),
                     std::set<Position, PositionCompare> & safeSpots) {
    for (auto it = safeSpots.begin(); it != safeSpots.end();) {
        auto nextPos = std::next(it, 1);
        if ((*canAttack)(p, *it)) {
            safeSpots.erase(it); // Invalidates it
        }
        it = nextPos;
    }
}

void putPiece(std::set<Position, PositionCompare> queenSpots,
              std::set<Position, PositionCompare> safeSpots, size_t & ways,
              std::set<std::bitset<SIZE * SIZE>, EncodingCompare> &
                  allValidPos) {
    // Stop when adequate queens are found or no safeSpots remain
    if (queenSpots.size() >= N) {
        // TODO Optimize by allowing any two flips and rotations equal
        auto encoding = encodeQueenPos(queenSpots);
        if (allValidPos.find(encoding) == allValidPos.end()) {
            std::cout << "way " << ++ways << ":\n";
            allValidPos.insert(encoding);
            printBoard(safeSpots, queenSpots);
            //std::cout << encoding.to_ullong() << std::endl;
        }
        return;
    }
    // Put the piece in each safe spot, see what happens
    for (auto it = safeSpots.begin(); it != safeSpots.end(); ++it) {
        // Copy the safe spots
        auto copyOfSafeSpots = safeSpots;
        auto copyOfQueenSpots = queenSpots;
        // Put queen at safe spot, and modify safe spots
        copyOfQueenSpots.insert(*it);
        updateSafeSpots(*it, queenAttack, copyOfSafeSpots);
        assert(copyOfSafeSpots.size() < safeSpots.size());
        putPiece(copyOfQueenSpots, copyOfSafeSpots, ways, allValidPos);
    }
}

// An approach that will work for any piece (real or customized through their
// attack pattern) and any number of them
size_t generalApproach() {
    std::set<Position, PositionCompare> safeSpots;
    std::set<Position, PositionCompare> queenSpots;
    for (int row = 0; row < SIZE; ++row) {
        for (int col = 0; col < SIZE; ++col) {
            safeSpots.insert(Position(row, col));
        }
    }
    size_t ways = 0;
    std::set<std::bitset<SIZE * SIZE>, EncodingCompare> allValidPos;
    putPiece(queenSpots, safeSpots, ways, allValidPos);
    return ways;
}

// Whether any queen currently placed can attack location p
bool isSafeFrom(const Position & p,
                const std::set<Position, PositionCompare> & queenSpots,
                bool (*canAttack)(const Position & src,
                                  const Position & target)) {
    for (auto it = queenSpots.begin(); it != queenSpots.end(); ++it) {
        if ((*canAttack)(*it, p)) {
            return false;
        }
    }
    return true;
}

void putQueen(std::set<Position, PositionCompare> queenSpots,
              std::set<size_t> remainingRows,
              const std::set<size_t> & remainingCols, size_t & ways) {
    if (remainingRows.empty()) {
        // Done, display result
        std::cout << "way " << ++ways << ":\n";;
        printBoard(queenSpots);
        return;
    }
    for (auto it = remainingCols.begin(); it != remainingCols.end(); ++it) {
        // If spot is peaceful (nothing in queenSpots can attack), recurse
        auto currentRow = *remainingRows.begin();
        auto currentCol = *it;
        if (isSafeFrom(Position(currentRow, currentCol), queenSpots,
                       queenAttack)) {
            auto copyOfRemainingRows = remainingRows;
            auto copyOfQueenSpots = queenSpots;
            copyOfQueenSpots.insert(Position(currentRow, currentCol));
            copyOfRemainingRows.erase(copyOfRemainingRows.begin());
            putQueen(copyOfQueenSpots, copyOfRemainingRows, remainingCols,
                     ways);
        }
    }
}

// Take advantage of fact that there are N queens on a N x N chessboard
size_t nQueensApproach() {
    // Each column and row must have no more and no less than one queen
    // Enumerate ways where queen is at (0,0), (0,1), (0,2) ...
    std::set<size_t> remainingRows, remainingCols;
    for (int i = 0; i < SIZE; ++i) {
        remainingRows.insert(i);
        remainingCols.insert(i);
    }
    std::set<Position, PositionCompare> queenSpots;
    size_t ways = 0;
    putQueen(queenSpots, remainingRows, remainingCols, ways);
    return ways;
}

int main() {
    assert(generalApproach() == 92); // Slow
    assert(nQueensApproach() == 92); // Fast (uses logic specific to Queens)
    return 0;
}
