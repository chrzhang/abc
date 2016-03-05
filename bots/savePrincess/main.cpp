#include <iostream>
#include <vector>
#include <string>
#include <cassert>
#include <cstdlib>
#include <ctime>

#define N 5

// Print directional instructions to reach a princess that lives at a corner of
// a square matrix with an odd width starting from the center

void displayPathtoPrincess(const std::vector<std::string> grid){
    int width = grid[0].size();
    assert(width % 2);
    int princessRow, princessCol;
    const std::string & firstRow = *grid.begin();
    const std::string & lastRow = *grid.rbegin();
    if (firstRow.front() == 'P') {
        princessRow = princessCol = 0;
    } else if (firstRow.back() == 'P') {
        princessRow = 0;
        princessCol = width - 1;
    } else if (lastRow.front() == 'P') {
        princessRow = grid.size() - 1;
        princessCol = 0;
    } else if (lastRow.back() == 'P') {
        princessRow = grid.size() - 1;
        princessCol = width - 1;
    } else {
        printf("No princess found.\n");
        return;
    }
    int centerRow, centerCol;
    centerRow = centerCol = width / 2;
    if (princessRow < centerRow) {
        for (int i = 0; i < centerRow - princessRow; ++i) {
            std::cout << "UP\n";
        }
    } else if (princessRow > centerRow) {
        for (int i = 0; i < princessRow - centerRow; ++i) {
            std::cout << "DOWN\n";
        }
    }
    if (princessCol < centerCol) {
        for (int i = 0; i < centerCol - princessCol; ++i) {
            std::cout << "LEFT\n";
        }
    } else if (princessCol > centerCol) {
        for (int i = 0; i < princessCol - centerCol; ++i) {
            std::cout << "RIGHT\n";
        }
    }
}

void printBoard(const std::vector<std::string> & board) {
    for (auto row : board) {
        for (auto e : row) {
            std::cout << e << " ";
        }
        std::cout << "\n";
    }
}

int main() {
    srand(time(0));
    std::vector<std::string> board(N, std::string(N, '*'));
    board[N / 2][N / 2] = 'O';
    switch (rand() % 4) {
        case 0:
            board[0][0] = 'P';
            break;
        case 1:
            board[0][N - 1] = 'P';
            break;
        case 2:
            board[N - 1][0] = 'P';
            break;
        case 3:
            board[N - 1][N - 1] = 'P';
            break;
    }
    printBoard(board);
    displayPathtoPrincess(board);
    return 0;
}
