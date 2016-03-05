#include <iostream>
#include <utility>
#include <list>
#include <vector>

#define M 20
#define N 20

int findNumNeighbors(const std::vector<std::vector<bool>> & board,
                     size_t rowIndex, size_t colIndex) {
    size_t width = board[0].size();
    // Dead cell with 3 live neighbors is born
    int numNeighbors = 0;
    // North
    if (rowIndex > 0) {
        if (board[rowIndex - 1][colIndex]) { ++numNeighbors; }
        if (colIndex < width - 1) { // Northeast
            if (board[rowIndex - 1][colIndex + 1]) { ++numNeighbors; }
        }
        if (colIndex > 0) { // Northwest
            if (board[rowIndex - 1][colIndex - 1]) { ++numNeighbors; }
        }
    }
    // East
    if (colIndex < width - 1) {
        if (board[rowIndex][colIndex + 1]) { ++numNeighbors; }
    }
    // West
    if (colIndex > 0) {
        if (board[rowIndex][colIndex - 1]) { ++numNeighbors; }
    }
    // South
    if (rowIndex < board.size() - 1) {
        if (board[rowIndex + 1][colIndex]) { ++numNeighbors; }
        if (colIndex < width - 1) { // Southeast
            if (board[rowIndex + 1][colIndex + 1]) { ++numNeighbors; }
        }
        if (colIndex > 0) { // Southwest
            if (board[rowIndex + 1][colIndex - 1]) { ++numNeighbors; }
        }
    }
    return numNeighbors;
}

void step(std::vector<std::vector<bool>> & board) {
    std::list<std::pair<int, int>> birthList;
    std::list<std::pair<int, int>> deathList;
    for (size_t rowIndex = 0; rowIndex < board.size(); ++rowIndex) {
        for (size_t colIndex = 0; colIndex < board[rowIndex].size();
             ++colIndex) {
            auto nn = findNumNeighbors(board, rowIndex, colIndex);
            if (board[rowIndex][colIndex]) {
                if (nn < 2) {
                    deathList.push_back(std::pair<int, int>(rowIndex,
                                                            colIndex));
                } else if (3 == nn || 2 == nn) {
                    // Lives on, do nothing
                } else if (nn > 3) {
                    deathList.push_back(std::pair<int, int>(rowIndex,
                                                            colIndex));
                }
            } else {
               if (3 == nn) {
                   birthList.push_back(std::pair<int, int>(rowIndex, colIndex));
               }
            }
        }
    }
    for (auto p : birthList) {
        board[p.first][p.second] = 1;
    }
    for (auto p : deathList) {
        board[p.first][p.second] = 0;
    }
}

void print(const std::vector<std::vector<bool>> & board) {
    for (auto row : board) {
        for (auto e : row) {
            std::cout << (e ? "* " : "  ");
        }
        std::cout << "\n";
    }
}

int main() {
    std::vector<std::vector<bool>> board(M, std::vector<bool>(N, false));
    // Glider
    board[1][2] = true;
    board[2][3] = true;
    board[3][1] = true;
    board[3][2] = true;
    board[3][3] = true;
    print(board);
    std::cout << "Hit <Enter> to perform a step.\n";
    std::string junk;
    while (std::getline(std::cin, junk)) {
        step(board);
        print(board);
    }
    return 0;
}
