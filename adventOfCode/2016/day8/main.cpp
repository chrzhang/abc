#include <iostream>
#include <fstream>
#include <string>
#include <cassert>
#include <sstream>
#include <regex>

#define SCREEN_ROW_COUNT 6
#define SCREEN_COL_COUNT 50

/*
From http://adventofcode.com/2016/day/8

--- Day 8: Two-Factor Authentication ---

You come across a door implementing what you can only assume is an
implementation of two-factor authentication after a long game of requirements
telephone.

To get past the door, you first swipe a keycard (no problem; there was one on a
nearby desk). Then, it displays a code on a little screen, and you type that
code on a keypad. Then, presumably, the door unlocks.

Unfortunately, the screen has been smashed. After a few minutes, you've taken
everything apart and figured out how it works. Now you just have to work out
what the screen would have displayed.

The magnetic strip on the card you swiped encodes a series of instructions for
the screen; these instructions are your puzzle input. The screen is 50 pixels
wide and 6 pixels tall, all of which start off, and is capable of three
somewhat peculiar operations:

rect AxB turns on all of the pixels in a rectangle at the top-left of the
screen which is A wide and B tall.  rotate row y=A by B shifts all of the
pixels in row A (0 is the top row) right by B pixels. Pixels that would fall
off the right end appear at the left end of the row.  rotate column x=A by B
shifts all of the pixels in column A (0 is the left column) down by B pixels.
Pixels that would fall off the bottom appear at the top of the column.  For
example, here is a simple sequence on a smaller screen:

rect 3x2 creates a small rectangle in the top-left corner:

###....
###....
.......

rotate column x=1 by 1 rotates the second column down by one pixel:

#.#....
###....
.#.....

rotate row y=0 by 4 rotates the top row right by four pixels:

....#.#
###....
.#.....

rotate column x=1 by 1 again rotates the second column down by one pixel,
causing the bottom pixel to wrap back to the top:

.#..#.#
#.#....
.#.....

As you can see, this display technology is extremely powerful, and will soon
dominate the tiny-code-displaying-screen market. That's what the advertisement
on the back of the display tries to convince you, anyway.

There seems to be an intermediate check of the voltage used by the display:
after you swipe your card, if the screen did work, how many pixels should be
lit?

--- Part Two ---

You notice that the screen is only capable of displaying capital letters; in
the font it uses, each letter is 5 pixels wide and 6 tall.

After you swipe your card, what code is the screen trying to display?
*/

void initScreen(bool screen[SCREEN_ROW_COUNT][SCREEN_COL_COUNT]) {
    for (int row = 0; row < SCREEN_ROW_COUNT; ++row) {
        for (int col = 0; col < SCREEN_COL_COUNT; ++col) {
            screen[row][col] = false;
        }
    }
}

void printScreen(bool screen[SCREEN_ROW_COUNT][SCREEN_COL_COUNT]) {
    for (int row = 0; row < SCREEN_ROW_COUNT; ++row) {
        for (int col = 0; col < SCREEN_COL_COUNT; ++col) {
            std::cout << (screen[row][col] ? '#' : '.') << " ";
        }
        std::cout << std::endl;
    }
}

int countTrueIn(bool screen[SCREEN_ROW_COUNT][SCREEN_COL_COUNT]) {
    int counter = 0;
    for (int row = 0; row < SCREEN_ROW_COUNT; ++row) {
        for (int col = 0; col < SCREEN_COL_COUNT; ++col) {
            if (screen[row][col]) {
                counter += 1;
            }
        }
    }
    return counter;
}

int toInt(const std::string & s) {
    std::stringstream ss(s);
    int result = 0;
    if (!(ss >> result)) {
        std::cerr << "Cannot convert " << s << " to int\n";
    }
    return result;
}

int shifted(const int x, const int amount, const int max) {
    return (x + amount) % max;
}

void fillRectangle(bool screen[SCREEN_ROW_COUNT][SCREEN_COL_COUNT],
                   const int numRows, const int numCols) {
    for (int row = 0; row < numRows; ++row) {
        for (int col = 0; col < numCols; ++col) {
            screen[row][col] = true;
        }
    }
}

void rotateRow(bool screen[SCREEN_ROW_COUNT][SCREEN_COL_COUNT],
               const int rowY, const int amount) {
    const int netAmount = amount % SCREEN_COL_COUNT;
    bool copyOfRow[SCREEN_COL_COUNT];
    for (int col = 0; col < SCREEN_COL_COUNT; ++col) {
        copyOfRow[col] = screen[rowY][col];
    }
    for (int col = 0; col < SCREEN_COL_COUNT; ++col) {
        screen[rowY][shifted(col, netAmount, SCREEN_COL_COUNT)] = copyOfRow[col];
    }
}

void rotateCol(bool screen[SCREEN_ROW_COUNT][SCREEN_COL_COUNT],
               const int colX, const int amount) {
    const int netAmount = amount % SCREEN_COL_COUNT;
    bool copyOfCol[SCREEN_ROW_COUNT];
    for (int row = 0; row < SCREEN_ROW_COUNT; ++row) {
        copyOfCol[row] = screen[row][colX];
    }
    for (int row = 0; row < SCREEN_ROW_COUNT; ++row) {
        screen[shifted(row, netAmount, SCREEN_ROW_COUNT)][colX] = copyOfCol[row];
    }
}

int main(int argc, char * argv[]) {
    if (2 != argc) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::ifstream inputFile(argv[1]);
    if (!inputFile.is_open()) {
        std::cerr << "Cannot open " << argv[1] << std::endl;
        return 1;
    }
    std::string currentLine;
    std::regex rectFillRegex("^rect ([0-9]+)x([0-9]+)$");
    std::regex rotateRowRegex("^rotate row y=([0-9]+) by ([0-9]+)$");
    std::regex rotateColRegex("^rotate column x=([0-9]+) by ([0-9]+)$");
    bool screen[SCREEN_ROW_COUNT][SCREEN_COL_COUNT];
    initScreen(screen);
    while (std::getline(inputFile, currentLine)) {
        std::smatch result;
        {
            std::regex_search(currentLine, result, rectFillRegex);
            if (3 == result.size()) {
                const auto & numberOfCols = toInt(result[1]);
                const auto & numberOfRows = toInt(result[2]);
                fillRectangle(screen, numberOfRows, numberOfCols);
            }
        }
        {
            std::regex_search(currentLine, result, rotateRowRegex);
            if (3 == result.size()) {
                const auto & rowY = toInt(result[1]);
                const auto & rotateAmount = toInt(result[2]);
                rotateRow(screen, rowY, rotateAmount);
            }
        }
        {
            std::regex_search(currentLine, result, rotateColRegex);
            if (3 == result.size()) {
                const auto & columnX = toInt(result[1]);
                const auto & rotateAmount = toInt(result[2]);
                rotateCol(screen, columnX, rotateAmount);
            }
        }
    }
    assert(countTrueIn(screen) == 115);
    std::cout << "Part 1: " << countTrueIn(screen) << std::endl;
    std::cout << "Part 2:\n";
    printScreen(screen);
    return 0;
}
