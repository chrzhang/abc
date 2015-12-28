#include <iostream>
#include <iomanip>
#include <bitset>

// Draw a horizontal line on a monochromatic screen of bytes

#define WIDTH 20
#define HEIGHT 10
#define INDENT 3

struct Screen {
    std::bitset<HEIGHT * WIDTH> pixels;
    bool access(size_t row, size_t col) const {
        return pixels[row * WIDTH + col];
    }
    void drawHorizontalLine(size_t x1, size_t x2, size_t y) {
        if (x1 > WIDTH - 1 || x2 > WIDTH - 1 || y > HEIGHT - 1) {
            std::cout << "Invalid coordinates.\n";
            return;
        }
        size_t startX, endX;
        if (x1 < x2) {
            startX = x1;
            endX = x2;
        } else {
            startX = x2;
            endX = x1;
        }
        size_t currX = startX;
        do {
            pixels[y * WIDTH + currX] = 1;
            ++currX;
        } while (currX != endX);
    }
};

std::ostream & operator<<(std::ostream & os, const Screen & s) {
    os << std::setw(INDENT) << " ";
    for (int i = 0; i < WIDTH; ++i) {
        os << std::setw(INDENT) << i;
    }
    os << std::endl;
    for (int row = 0; row < HEIGHT; ++row) {
        os << std::setw(INDENT) << row;
        for (int col = 0; col < WIDTH; ++ col) {
            if (s.access(row, col)) {
                os << std::setw(INDENT) << "-";
            } else {
                os << std::setw(INDENT) << " ";
            }
        }
        os << std::endl;
    }
    return os;
}

int main() {
    Screen s;
    s.drawHorizontalLine(4, 10, 7);
    std::cout << s << std::endl;
    return 0;
}
