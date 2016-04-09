#include <iostream>
#include <iomanip>
#include <cassert>
#include <stdexcept>

#define INDENT 3

// Print and allow input into 7 x 6 Connect 4 board

enum PlayerColor { red, black };

struct Board {
    char board[6][7];
    int emptyTopRowForEachCol[7];
    bool hasWinner = false;
    Board() {
        for (int r = 0; r < 6; ++r) {
            for (int c = 0; c < 7; ++c) {
                board[r][c] = ' ';
            }
        }
        for (int c = 0; c < 7; ++c) {
            emptyTopRowForEachCol[c] = 5;
        }
    }
    bool inBoundsRow(int row) {
        return row >= 0 && row <= 5;
    }
    bool inBoundsCol(int col) {
        return col >= 0 && col <= 6;
    }
    int getAmtOfSameColor(int rowDiff, int colDiff, int row, int col,
                          const PlayerColor & color) {
        assert(inBoundsRow(row) && inBoundsCol(col));
        col += colDiff;
        row += rowDiff;
        int r = 0;
        while (inBoundsRow(row) && inBoundsCol(col)) {
            if (board[row][col] != (color == red ? 'r' : 'b')) { break; }
            ++r;
            col += colDiff;
            row += rowDiff;
        }
        return r;
    }
    int getAmtOfSameColorW(int row, int col, const PlayerColor & color) {
        return getAmtOfSameColor(0, -1, row, col, color);
    }
    int getAmtOfSameColorE(int row, int col, const PlayerColor & color) {
        return getAmtOfSameColor(0, +1, row, col, color);
    }
    int getAmtOfSameColorN(int row, int col, const PlayerColor & color) {
        return getAmtOfSameColor(-1, 0, row, col, color);
    }
    int getAmtOfSameColorS(int row, int col, const PlayerColor & color) {
        return getAmtOfSameColor(+1, 0, row, col, color);
    }
    int getAmtOfSameColorNW(int row, int col, const PlayerColor & color) {
        return getAmtOfSameColor(-1, -1, row, col, color);
    }
    int getAmtOfSameColorNE(int row, int col, const PlayerColor & color) {
        return getAmtOfSameColor(-1, +1, row, col, color);
    }
    int getAmtOfSameColorSW(int row, int col, const PlayerColor & color) {
        return getAmtOfSameColor(+1, -1, row, col, color);
    }
    int getAmtOfSameColorSE(int row, int col, const PlayerColor & color) {
        return getAmtOfSameColor(+1, +1, row, col, color);
    }
    int putInAt(int column, const PlayerColor & color) {
        if (hasWinner) { return -1; }
        if (!inBoundsCol(column)) { return -1; }
        if (emptyTopRowForEachCol[column] < 0) { return -1; }
        int row = emptyTopRowForEachCol[column];
        board[row][column] = (color == red ? 'r' : 'b');
        --emptyTopRowForEachCol[column];
        // Check if current color won
        // Go east and west
        int sameColorW = getAmtOfSameColorW(row, column, color);
        int sameColorE = getAmtOfSameColorE(row, column, color);
        if (sameColorW + sameColorE + 1 >= 4) {
            hasWinner = true;
            std::cout << "You win!\n";
            return 1;
        }
        // Go north and south
        int sameColorN = getAmtOfSameColorN(row, column, color);
        int sameColorS = getAmtOfSameColorS(row, column, color);
        if (sameColorN + sameColorS + 1 >= 4) {
            hasWinner = true;
            std::cout << "You win!\n";
            return 1;
        }
        // Go northwest and southeast
        int sameColorNW = getAmtOfSameColorNW(row, column, color);
        int sameColorSE = getAmtOfSameColorSE(row, column, color);
        if (sameColorNW + sameColorSE + 1 >= 4) {
            hasWinner = true;
            std::cout << "You win!\n";
            return 1;
        }
        // Go northeast and southwest
        int sameColorNE = getAmtOfSameColorNE(row, column, color);
        int sameColorSW = getAmtOfSameColorSW(row, column, color);
        if (sameColorNE + sameColorSW + 1 >= 4) {
            hasWinner = true;
            std::cout << "You win!\n";
            return 1;
        }
        return 0;
    }
};

std::ostream & operator<<(std::ostream & os, const Board & b) {
    // Print top indices
    os << std::setw(INDENT) << " ";
    for (int i = 0; i < 7; ++i) {
        os << std::setw(INDENT) << i;
    }
    os << "\n";
    // Print rows
    for (int r = 0; r < 6; ++r) {
        os << std::setw(INDENT) << r;
        for (int c = 0; c < 7; ++c) {
            os << std::setw(INDENT) << b.board[r][c];
        }
        os << "\n";
    }
    return os;
}

int main() {
    Board b;
    std::cout << b << std::endl;
    PlayerColor c = red;
    std::string input;
    for (;;) {
        std::cout << "As " << (c == red ? "red" : "black")
                  << ", choose a col\n";
        std::getline(std::cin, input);
        try {
            int r = b.putInAt(std::stoi(input, nullptr, 0), c);
            if (-1 == r) {
                std::cout << "Could not put in piece in.\n";
            } else if (1 == r) {
                std::cout << b << std::endl;
                return 0;
            } else {
                std::cout << b << std::endl;
                c = (c == red ? black : red);
            }
        } catch (const std::invalid_argument & ia) {
            std::cout << "Column invalid. Could not put in piece in.\n";
        }
    }
    return 0;
}
