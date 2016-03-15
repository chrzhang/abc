#include <iostream>
#include <vector>
#include <cassert>
#include <list>

// Solve a Sudoku puzzle

struct Candidates {
    bool possible[9];
    int numPossible;
    Candidates() {
        for (int i = 0; i < 9; ++i) { possible[i] = true; }
        numPossible = 9;
    }
    // Returns 0 normally, but if one candidate is left, returns that candidate
    int setFalse(int i) {
        assert(i >= 1 && i <= 9);
        if (possible[i - 1]) {
            if (numPossible == 1) { assert(false); }
            possible[i - 1] = false;
            --numPossible;
            if (1 == numPossible) {
                for (int i = 0; i < 9; ++i) {
                    if (possible[i]) {
                        return i + 1;
                    }
                }
            }
        }
        return 0;
    }
};

void printGrid(char grid[9][9]) {
    std::cout << "=== Grid ===\n";
    for (int row = 0; row < 9; ++row) {
        for (int col = 0; col < 9; ++col) {
            std::cout << grid[row][col] << " ";
            if (col == 2 || col == 5) { std::cout << " "; }
        }
        std::cout << "\n";
        if (row == 2 || row == 5) { std::cout << "\n"; }
    }
}

void printNumPossibilities(Candidates grid[9][9]) {
    std::cout << "=== # of Possibilities ===\n";
    for (int row = 0; row < 9; ++row) {
        for (int col = 0; col < 9; ++col) {
            std::cout << grid[row][col].numPossible << " ";
            if (col == 2 || col == 5) { std::cout << " "; }
        }
        std::cout << "\n";
        if (row == 2 || row == 5) { std::cout << "\n"; }
    }
}

int getAnchor(int x) {
    return 3 * (x / 3);
}

void process(int row, int col, char grid[9][9], Candidates gridC[9][9],
             std::list<std::pair<int, int>> & pointsOfInterest) {
    if (!isdigit(grid[row][col])) { return; }
    auto c = grid[row][col] - '1' + 1;
    // Remove possibility from the row, col, and current box
    for (int i = 0; i < 9; ++i) {
        if (i == col) { continue; }
        auto r = gridC[row][i].setFalse(c);
        if (r) {
            grid[row][i] = (r - 1) + '1';
            pointsOfInterest.push_back(std::pair<int, int>(row, i));
        }
    }
    for (int i = 0; i < 9; ++i) {
        if (i == row) { continue; }
        auto r = gridC[i][col].setFalse(c);
        if (r) {
            grid[i][col] = (r - 1) + '1';
            pointsOfInterest.push_back(std::pair<int, int>(i, col));
        }
    }
    int m = getAnchor(row);
    int n = getAnchor(col);
    for (int k = 0; k < 3; ++k) {
        for (int l = 0; l < 3; ++l) {
            if (m + k == row && n + l == col) { continue; }
            auto r = gridC[m + k][n + l].setFalse(c);
            if (r) {
                grid[m + k][n + l] = (r - 1) + '1';
                pointsOfInterest.push_back(std::pair<int, int>(m + k, n + l));
            }
        }
    }
}

int solve(char grid[9][9]) {
    Candidates gridC[9][9];
    std::list<std::pair<int, int>> pointsOfInterest;
    for (int row = 0; row < 9; ++row) {
        for (int col = 0; col < 9; ++col) {
            pointsOfInterest.push_back(std::pair<int, int>(row, col));
        }
    }
    while (!pointsOfInterest.empty()) {
        int row = pointsOfInterest.front().first;
        int col = pointsOfInterest.front().second;
        pointsOfInterest.pop_front();
        process(row, col, grid, gridC, pointsOfInterest);
    }
    printNumPossibilities(gridC);
    return 0;
}

bool matches(char g1[9][9], char g2[9][9]) {
    for (int row = 0; row < 9; ++row) {
        for (int col = 0; col < 9; ++col) {
            if (g1[row][col] != g2[row][col]) { return false; }
        }
    }
    return true;
}

int main() {
    /*
    char grid[9][9] = { {'-', '-', '-',  '-', '-', '-',  '-', '-', '-'},
                        {'-', '-', '-',  '-', '-', '-',  '-', '-', '-'},
                        {'-', '-', '-',  '-', '-', '-',  '-', '-', '-'},

                        {'-', '-', '-',  '-', '-', '-',  '-', '-', '-'},
                        {'-', '-', '-',  '-', '-', '-',  '-', '-', '-'},
                        {'-', '-', '-',  '-', '-', '-',  '-', '-', '-'},

                        {'-', '-', '-',  '-', '-', '-',  '-', '-', '-'},
                        {'-', '-', '-',  '-', '-', '-',  '-', '-', '-'},
                        {'-', '-', '-',  '-', '-', '-',  '-', '-', '-'} };
    */
    // This puzzle will be solved by constraining the search space
    char grid[9][9] = { {'-', '-', '3',  '-', '2', '-',  '6', '-', '-'},
                        {'9', '-', '-',  '3', '-', '5',  '-', '-', '1'},
                        {'-', '-', '1',  '8', '-', '6',  '4', '-', '-'},

                        {'-', '-', '8',  '1', '-', '2',  '9', '-', '-'},
                        {'7', '-', '-',  '-', '-', '-',  '-', '-', '8'},
                        {'-', '-', '6',  '7', '-', '8',  '2', '-', '-'},

                        {'-', '-', '2',  '6', '-', '9',  '5', '-', '-'},
                        {'8', '-', '-',  '2', '-', '3',  '-', '-', '9'},
                        {'-', '-', '5',  '-', '1', '-',  '3', '-', '-'} };
    printGrid(grid);
    solve(grid);
    printGrid(grid);
    // Not all puzzles can be solved with this method
    // TODO Use a recursive DFS trial of each possible filler to solve all
    // problems
    return 0;
}
