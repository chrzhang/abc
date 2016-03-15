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
            if (numPossible == 1) { return -1; }
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

// Given a square's row or column index, find the index of the NW corner of the
// 3 x 3 box it lives in
int getAnchor(int x) {
    return 3 * (x / 3);
}

bool isSudokuDigit(char c) {
    return c >= '1' && c <= '9';
}

// Return -1 on error (a square no longer has possibilities)
int process(int row, int col, char grid[9][9], Candidates gridC[9][9],
             std::list<std::pair<int, int>> & pointsOfInterest) {
    if (!isSudokuDigit(grid[row][col])) { return 0; }
    auto c = grid[row][col] - '1' + 1;
    // Remove possibility from the row, col, and current box
    for (int i = 0; i < 9; ++i) {
        if (i == col) { continue; }
        auto r = gridC[row][i].setFalse(c);
        if (r > 0 && !isSudokuDigit(grid[row][i])) {
            grid[row][i] = (r - 1) + '1';
            pointsOfInterest.push_back(std::pair<int, int>(row, i));
        } else if (r == -1) {
            return -1;
        }
    }
    for (int i = 0; i < 9; ++i) {
        if (i == row) { continue; }
        auto r = gridC[i][col].setFalse(c);
        if (r > 0 && !isSudokuDigit(grid[i][col])) {
            grid[i][col] = (r - 1) + '1';
            pointsOfInterest.push_back(std::pair<int, int>(i, col));
        } else if (r == -1) {
            return -1;
        }
    }
    int m = getAnchor(row);
    int n = getAnchor(col);
    for (int k = 0; k < 3; ++k) {
        for (int l = 0; l < 3; ++l) {
            if (m + k == row && n + l == col) { continue; }
            auto r = gridC[m + k][n + l].setFalse(c);
            if (r > 0 && !isSudokuDigit(grid[m + k][n + l])) {
                grid[m + k][n + l] = (r - 1) + '1';
                pointsOfInterest.push_back(std::pair<int, int>(m + k, n + l));
            } else if (r == -1) {
                return -1;
            }
        }
    }
    return 0;
}

void commit(char grid[9][9], char finalGrid[9][9]) {
    for (int r = 0; r < 9; ++r) {
        for (int c = 0; c < 9; ++c) {
            finalGrid[r][c] = grid[r][c];
        }
    }
}

int trySolve(char grid[9][9], Candidates gridC[9][9], char finalGrid[9][9]) {
    bool completed = true;
    // Try every candidate by recursively deepening into possible configurations
    for (int row = 0; row < 9; ++row) {
        for (int col = 0; col < 9; ++col) {
            if (!isSudokuDigit(grid[row][col])) {
                completed = false;
                Candidates & cands = gridC[row][col];
                for (int i = 0; i < 9; ++i) {
                    if (cands.possible[i]) {
                        // Make copies
                        char x_grid[9][9];
                        Candidates x_gridC[9][9];
                        for (int r = 0; r < 9; ++r) {
                            for (int c = 0; c < 9; ++c) {
                                x_grid[r][c] = grid[r][c];
                                x_gridC[r][c] = gridC[r][c];
                            }
                        }
                        x_grid[row][col] = i + '1';
                        std::list<std::pair<int, int>> pointsOfInterest;
                        pointsOfInterest.push_back(
                            std::pair<int, int>(row, col));
                        while (!pointsOfInterest.empty()) {
                            int row = pointsOfInterest.front().first;
                            int col = pointsOfInterest.front().second;
                            pointsOfInterest.pop_front();
                            auto r = process(row, col, x_grid, x_gridC,
                                             pointsOfInterest);
                            if (-1 == r) { return -1; }
                        }
                        if (1 == trySolve(x_grid, x_gridC, finalGrid)) {
                            return 1;
                        }
                    }
                }
            }
        }
    }
    if (completed) {
        commit(grid, finalGrid);
        return 1;
    }
    return 0;
}

int solve(char grid[9][9]) {
    // Begin by constraining possibilities (some easier puzzles can be solved
    // completely merely with this method)
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
    // Recursively descend and entertain possibilities from our constrained
    // sample space (further constraining on copies of candidates)
    trySolve(grid, gridC, grid);
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
    // Arto Inkala's 2010 puzzle
    char grid[9][9] = { {'-', '-', '5',  '3', '-', '-',  '-', '-', '-'},
                        {'8', '-', '-',  '-', '-', '-',  '-', '2', '-'},
                        {'-', '7', '-',  '-', '1', '-',  '5', '-', '-'},

                        {'4', '-', '-',  '-', '-', '5',  '3', '-', '-'},
                        {'-', '1', '-',  '-', '7', '-',  '-', '-', '6'},
                        {'-', '-', '3',  '2', '-', '-',  '-', '8', '-'},

                        {'-', '6', '-',  '5', '-', '-',  '-', '-', '9'},
                        {'-', '-', '4',  '-', '-', '-',  '-', '3', '-'},
                        {'-', '-', '-',  '-', '-', '9',  '7', '-', '-'} };
    // Puzzle can be solved by recursively entertaining all solutions but it
    // will be benefited by constraining the search space before guess and
    // checking every plausible option
    printGrid(grid);
    solve(grid);
    printGrid(grid);
    char gridAnswer[9][9] = { {'1', '4', '5',  '3', '2', '7',  '6', '9', '8'},
                              {'8', '3', '9',  '6', '5', '4',  '1', '2', '7'},
                              {'6', '7', '2',  '9', '1', '8',  '5', '4', '3'},

                              {'4', '9', '6',  '1', '8', '5',  '3', '7', '2'},
                              {'2', '1', '8',  '4', '7', '3',  '9', '5', '6'},
                              {'7', '5', '3',  '2', '9', '6',  '4', '8', '1'},

                              {'3', '6', '7',  '5', '4', '2',  '8', '1', '9'},
                              {'9', '8', '4',  '7', '6', '1',  '2', '3', '5'},
                              {'5', '2', '1',  '8', '3', '9',  '7', '6', '4'} };
    assert(matches(grid, gridAnswer));
    return 0;
}
