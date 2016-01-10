#include <iostream>
#include <cstdlib>
#include <ctime>

#define N 3
#define NUM_ITERATIONS 100

// Check if someone has won tic tac toe

enum Tile { o, x, empty };

void printBoard(Tile board[N][N]) {
    for (int row = 0; row < N; ++row) {
        for (int col = 0; col < N; ++col) {
            if (board[row][col] == o) {
                std::cout << "O ";
            } else if (board[row][col] == x) {
                std::cout << "X ";
            } else {
                std::cout << "  ";
            }
        }
        std::cout << std::endl;
    }
}

bool matchesRow(int row, int col, Tile board[N][N]) {
    for (int i = col; i < N; ++i) {
        if (board[row][i] != board[row][col]) {
            return false;
        }
    }
    return true;
}

bool matchesCol(int row, int col, Tile board[N][N]) {
    for (int i = row; i < N; ++i) {
        if (board[i][col] != board[row][col]) {
            return false;
        }
    }
    return true;
}

bool matchesDiagR(int row, int col, Tile board[N][N]) {
    for (int i = 0; i < N; ++i) {
        if (board[i][i] != board[row][col]) {
            return false;
        }
    }
    return true;
}

bool matchesDiagL(int row, int col, Tile board[N][N]) {
    for (int i = 0; i < N; ++i) {
        if (board[i][(N - 1) - i] != board[row][col]) {
            return false;
        }
    }
    return true;
}

Tile whoWon(Tile board[N][N]) { // Returns whether O or X or nobody (empty) won
    // Check for right
    for (int row = 0; row < N; ++row) {
        if (matchesRow(row, 0, board)) {
            if (board[row][0] != empty) {
                return board[row][0];
            }
        }
    }
    // Check for down
    for (int col = 0; col < N; ++col) {
        if (matchesCol(0, col, board)) {
            if (board[0][col] != empty) {
                return board[0][col];
            }
        }
    }
    // Check for diagonal top-right
    if (matchesDiagR(0, 0, board)) {
        if (board[0][0] != empty) {
            return board[0][0];
        }
    }
    // Check for diagonal top-left
    if (matchesDiagL(0, N - 1, board)) {
        if (board[0][N - 1] != empty) {
            return board[0][N - 1];
        }
    }
    return empty;
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS;) {
        Tile ticTacToeBoard[N][N];
        for (int row = 0; row < N; ++row) {
            for (int col = 0; col < N; ++col) {
                switch(rand() % N) {
                    case 0: {
                        ticTacToeBoard[row][col] = empty;
                        break;
                    }
                    case 1: {
                        ticTacToeBoard[row][col] = o;
                        break;
                    }
                    case 2: {
                        ticTacToeBoard[row][col] = x;
                        break;
                    }
                }
            }
        }
        auto winner = whoWon(ticTacToeBoard);
        if (winner != empty) {
            if (winner == o) {
                std::cout << "=== O won: \n";
            } else {
                std::cout << "=== X won: \n";
            }
            printBoard(ticTacToeBoard);
            ++iteration;
        }
    }
    return 0;
}
