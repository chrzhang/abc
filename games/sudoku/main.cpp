#include <iostream>
#include <vector>
#include <cstdlib>
#include <ctime>

#define NUMPUZZLES 5

// Check if random arrangements of a Sudoku puzzle are valid

void printBoard(const std::vector<std::vector<char>> & board) {
    for (auto row : board) {
        for (auto e : row) {
            std::cout << e << " ";
        }
        std::cout << std::endl;
    }
}

bool isValidSudoku(std::vector<std::vector<char>>& board) {
    // Check each row
    for (auto row : board) {
        bool a[9] = { false };
        for (auto e : row) {
            if (e == '.') { continue; }
            if (a[e - '1']) { return false; }
            a[e - '1'] = true;
        }
    }
    // Check each col
    for (int col = 0; col < 9; ++ col) {
        bool a[9] = { false };
        for (auto row : board) {
            if (row[col] == '.') { continue; }
            if (a[row[col] - '1']) { return false; }
            a[row[col] - '1'] = true;
        }
    }
    // Check each 3 x 3 box
    for (int boxRow = 0; boxRow < 3; ++boxRow) {
        for (int boxCol = 0; boxCol < 3; ++boxCol) {
            bool a[9] = { false };
            for (int row = boxRow * 3; row < boxRow * 3 + 3; ++row) {
                for (int col = boxCol * 3; col < boxCol * 3 + 3; ++col) {
                    if (board[row][col] == '.') { continue; }
                    if (a[board[row][col] - '1']) { return false; }
                    a[board[row][col] - '1'] = true;
                }
            }
        }
    }
    return true;
}

int main() {
    srand(time(0));
    for (int i = 0; i < NUMPUZZLES;) {
        std::vector<std::vector<char>> board;
        for (int i = 0; i < 9; ++i) {
            std::vector<char> v;
            for (int j = 0; j < 9; ++j) {
                auto r = rand() % 20;
                v.push_back((r >= 1 && r <= 9) ? '0' + r : '.');
            }
            board.push_back(v);
        }
        if (isValidSudoku(board)) {
            std::cout << "Puzzle " << i << ":\n";
            printBoard(board);
            ++i;
        }
    }
    return 0;
}
