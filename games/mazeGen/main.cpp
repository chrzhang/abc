#include <iostream>
#include <stack>
#include <vector>
#include <cassert>
#include <cstdlib>
#include <ctime>
#include <set>

#define HEIGHT 20
#define WIDTH 20

// Generate and print a maze

int min(int a, int b) {
    return a < b ? a : b;
}

struct Maze {
    // Maze is visualized as a 2D array with padding in between neighbors to
    // draw walls so {0,0} is at (0,0), {0,1} is at (0,2), {1,0} is at (2,0)
    bool maze[2 * HEIGHT - 1][2 * WIDTH - 1]; // Let us print walls fluidly
    void setIndexTo(int row, int col, bool val) {
        if (2 * row < 0 || 2 * row >= 2 * HEIGHT - 1) { return; }
        if (2 * col < 0 || 2 * col >= 2 * WIDTH - 1) { return; }
        maze[2 * row][2 * col] = val;
    }
    int getValAt(int trueRow, int trueCol) const {
        if (trueRow < 0 || trueRow >= 2 * HEIGHT - 1) { return -1; }
        if (trueCol < 0 || trueCol >= 2 * WIDTH - 1) { return -1; }
        return maze[trueRow][trueCol] ? 1 : 0;
    }
    void makePathBetween(const std::pair<int, int> & ij1,
                         const std::pair<int, int> & ij2) {
        auto r1 = ij1.first;
        auto r2 = ij2.first;
        auto c1 = ij1.second;
        auto c2 = ij2.second;
        setIndexTo(r1, c1, true);
        setIndexTo(r2, c2, true);
        if (r1 == r2) {
            maze[2 * r1][2 * min(c1, c2) + 1] = true;
        } else {
            assert(c1 == c2);
            maze[2 * min(r1, r2) + 1][2 * c1] = true;
        }
    }
    bool isValidIndexing(const std::pair<int, int> & ij) const {
        if (ij.first < 0 || ij.first >= HEIGHT) { return false; }
        if (ij.second < 0 || ij.second >= WIDTH) { return false; }
        return true;
    }
    bool isUnvisited(const std::pair<int, int> & ij) const {
        return (0 == getValAt(ij.first * 2, ij.second * 2));
    }
    std::pair<int, int> getRandomUnvisitedNeighbor(
                            const std::pair<int, int> & ij) const {
        std::pair<int, int> r(-1, -1);
        std::vector<std::pair<int, int>> possibleUnvisitedNeighbors;
        // Check north, east, west, and south
        auto north = std::pair<int, int>(ij.first - 1, ij.second);
        auto east = std::pair<int, int>(ij.first, ij.second + 1);
        auto west = std::pair<int, int>(ij.first, ij.second - 1);
        auto south = std::pair<int, int>(ij.first + 1, ij.second);
        if (isValidIndexing(north) && isUnvisited(north)) {
            possibleUnvisitedNeighbors.push_back(north);
        }
        if (isValidIndexing(east) && isUnvisited(east)) {
            possibleUnvisitedNeighbors.push_back(east);
        }
        if (isValidIndexing(west) && isUnvisited(west)) {
            possibleUnvisitedNeighbors.push_back(west);
        }
        if (isValidIndexing(south) && isUnvisited(south)) {
            possibleUnvisitedNeighbors.push_back(south);
        }
        if (!possibleUnvisitedNeighbors.empty()) {
            r = possibleUnvisitedNeighbors[rand() %
                                           possibleUnvisitedNeighbors.size()];
        }
        return r;
    }
    void makeMazeDFSBacktracking() {
        // Starts top left
        std::stack<std::pair<int, int>> s;
        s.push(std::pair<int, int>(0, 0));
        while (!s.empty()) {
            auto top = s.top();
            auto unvisitedNeighbor = getRandomUnvisitedNeighbor(top);
            if (unvisitedNeighbor.first == -1) { // No unvisited neighbors
                assert(unvisitedNeighbor.second == -1);
                s.pop();
            } else {
                s.push(unvisitedNeighbor);
                makePathBetween(top, unvisitedNeighbor);
            }
        }
    }
    void addUnvisitedNeighborsToFrontier(const std::pair<int, int> & ij,
                                         std::set<std::pair<int, int>> &
                                            frontier) {
        auto north = std::pair<int, int>(ij.first - 1, ij.second);
        auto east = std::pair<int, int>(ij.first, ij.second + 1);
        auto west = std::pair<int, int>(ij.first, ij.second - 1);
        auto south = std::pair<int, int>(ij.first + 1, ij.second);
        if (isValidIndexing(north) && isUnvisited(north)) {
            frontier.insert(north);
        }
        if (isValidIndexing(east) && isUnvisited(east)) {
            frontier.insert(east);
        }
        if (isValidIndexing(west) && isUnvisited(west)) {
            frontier.insert(west);
        }
        if (isValidIndexing(south) && isUnvisited(south)) {
            frontier.insert(south);
        }
    }
    std::pair<int, int> popRandomFrontierCell(std::set<std::pair<int, int>> &
                                                frontier) {
        assert(!frontier.empty());
        auto it = frontier.begin();
        std::advance(it, rand() % frontier.size());
        auto copyOfValue = *it;
        frontier.erase(it);
        assert(isUnvisited(copyOfValue));
        return copyOfValue;
    }
    std::pair<int, int> getRandomMazeCellAdjTo(const std::pair<int, int> & ij) {
        std::vector<std::pair<int, int>> possibleAdjMazeCells;
        auto north = std::pair<int, int>(ij.first - 1, ij.second);
        auto east = std::pair<int, int>(ij.first, ij.second + 1);
        auto west = std::pair<int, int>(ij.first, ij.second - 1);
        auto south = std::pair<int, int>(ij.first + 1, ij.second);
        if (isValidIndexing(north) && maze[2 * north.first][2 * north.second]) {
            possibleAdjMazeCells.push_back(north);
        }
        if (isValidIndexing(east) && maze[2 * east.first][2 * east.second]) {
            possibleAdjMazeCells.push_back(east);
        }
        if (isValidIndexing(west) && maze[2 * west.first][2 * west.second]) {
            possibleAdjMazeCells.push_back(west);
        }
        if (isValidIndexing(south) && maze[2 * south.first][2 * south.second]) {
            possibleAdjMazeCells.push_back(south);
        }
        assert(!possibleAdjMazeCells.empty());
        return possibleAdjMazeCells[rand() % possibleAdjMazeCells.size()];
    }
    void makeMazePrimsRandomized() {
        std::pair<int, int> start(0, 0); // Could be random cell
        maze[start.first][start.second] = true;
        std::set<std::pair<int, int>> frontier;
        addUnvisitedNeighborsToFrontier(start, frontier);
        while (!frontier.empty()) {
            auto randomCell = popRandomFrontierCell(frontier);
            auto cellInMazeAdjacentToRandomCell = getRandomMazeCellAdjTo(randomCell);
            makePathBetween(randomCell, cellInMazeAdjacentToRandomCell);
            addUnvisitedNeighborsToFrontier(randomCell, frontier);
        }
    }
    Maze() {
        for (int row = 0; row < 2 * HEIGHT - 1; ++row) {
            for (int col = 0; col < 2 * WIDTH - 1; ++col) {
                maze[row][col] = false;
            }
        }
        //makeMazeDFSBacktracking();
        makeMazePrimsRandomized();
    }
};

std::ostream & operator<<(std::ostream & os, const Maze & m) {
    os << "+ ";
    for (int col = 0; col < 2 * WIDTH - 1 ; ++col) {
        os << "- ";
    }
    os << "+ ";
    os << std::endl;
    for (int row = 0; row < 2 * HEIGHT - 1; ++row) {
        os << "| ";
        for (int col = 0; col < 2 * WIDTH - 1; ++col) {
            os << (m.getValAt(row, col) ? "  " : "* ");
        }
        os << "| ";
        os << std::endl;
    }
    os << "+ ";
    for (int col = 0; col < 2 * WIDTH - 1; ++col) {
        os << "- ";
    }
    os << "+ ";
    os << std::endl;

    return os;
}

int main() {
    srand(time(0));
    Maze m;
    std::cout << m << std::endl;
    return 0;
}
