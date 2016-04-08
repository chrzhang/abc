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
    int putInAt(int column, const PlayerColor & color) {
        if (column < 0 || column > 6) { return -1; }
        if (emptyTopRowForEachCol[column] < 0) { return -1; }
        board[emptyTopRowForEachCol[column]][column] =
            (color == red ? 'r' : 'b');
        --emptyTopRowForEachCol[column];
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
            if (-1 == b.putInAt(std::stoi(input, nullptr, 0), c)) {
                std::cout << "Could not put in piece in.\n";
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
