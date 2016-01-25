#include <iostream>
#include <stack>
#include <vector>
#include <cassert>
#include <cstdlib>
#include <ctime>

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
    bool isUnvisited(const std::pair<int, int> & ij,
                     bool visited[HEIGHT][WIDTH]) const {
        return (false == visited[ij.first][ij.second]);
    }
    std::pair<int, int> getRandomUnvisitedNeighbor(
                            const std::pair<int, int> & ij,
                            bool visited[HEIGHT][WIDTH]) const {
        std::pair<int, int> r(-1, -1);
        std::vector<std::pair<int, int>> possibleUnvisitedNeighbors;
        // Check north, east, west, and south
        auto north = std::pair<int, int>(ij.first - 1, ij.second);
        if (isValidIndexing(north) && isUnvisited(north, visited)) {
            possibleUnvisitedNeighbors.push_back(north);
        }
        auto east = std::pair<int, int>(ij.first, ij.second + 1);
        if (isValidIndexing(east) && isUnvisited(east, visited)) {
            possibleUnvisitedNeighbors.push_back(east);
        }
        auto west = std::pair<int, int>(ij.first, ij.second - 1);
        if (isValidIndexing(west) && isUnvisited(west, visited)) {
            possibleUnvisitedNeighbors.push_back(west);
        }
        auto south = std::pair<int, int>(ij.first + 1, ij.second);
        if (isValidIndexing(south) && isUnvisited(south, visited)) {
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
        bool visited[HEIGHT][WIDTH];
        for (int i = 0; i < HEIGHT; ++i) {
            for (int j = 0; j < WIDTH; ++j)  {
                visited[i][j] = false;
            }
        }
        s.push(std::pair<int, int>(0, 0));
        while (!s.empty()) {
            auto top = s.top();
            visited[top.first][top.second] = true;
            auto unvisitedNeighbor = getRandomUnvisitedNeighbor(top, visited);
            if (unvisitedNeighbor.first == -1) {
                assert(unvisitedNeighbor.second == -1);
                s.pop();
            } else {
                s.push(unvisitedNeighbor);
                makePathBetween(top, unvisitedNeighbor);
            }
        }
    }
    Maze() {
        for (int row = 0; row < 2 * HEIGHT - 1; ++row) {
            for (int col = 0; col < 2 * WIDTH - 1; ++col) {
                maze[row][col] = false;
            }
        }
        makeMazeDFSBacktracking();
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
