#include <iostream>
#include <cstdlib>
#include <ctime>
#include <assert.h>

#define SLOTS 4

// A game is configured with a solution having 4 ordered slots of colored balls
// so given a guess, find the # of matches and psuedo-matches (wrong slot but
// color exists in solution)

enum Ball { red, green, blue, yellow, Ball_MAX = yellow };

struct Result {
    unsigned numMatches;
    unsigned numPseudoMatches;
    Result() {
        numMatches = numPseudoMatches = 0;
    }
};

void printSlots(Ball solution[SLOTS]) {
    std::cout << "[ ";
    for (int i = 0; i < SLOTS; ++i) {
        switch (solution[i]) {
            case red: {
                std::cout << "R ";
                break;
            }
            case green: {
                std::cout << "G ";
                break;
            }
            case blue: {
                std::cout << "B ";
                break;
            }
            case yellow: {
                std::cout << "Y ";
                break;
            }
        }
    }
    std::cout << "]" << std::endl;
}

void fillRandomly(Ball m[SLOTS]) {
    for (int i = 0; i < SLOTS; ++i) {
        switch (rand() % SLOTS) {
            case 0: {
                m[i] = red;
                break;
            }
            case 1: {
                m[i] = green;
                break;
            }
            case 2: {
                m[i] = blue;
                break;
            }
            case 3: {
                m[i] = yellow;
                break;
            }
        }
    }
}

Result findResult(Ball guess[SLOTS], Ball solution[SLOTS]) {
    Result r;
    bool colorsFoundInSolution[Ball_MAX + 1];
    for (int i = 0; i < Ball_MAX + 1; ++i) {
        colorsFoundInSolution[i] = false;
    }
    for (int i = 0; i < SLOTS; ++i) {
        colorsFoundInSolution[solution[i]] = true;
    }
    for (int i = 0; i < SLOTS; ++i) {
        if (guess[i] == solution[i]) {
            ++r.numMatches;
        } else if (colorsFoundInSolution[guess[i]]) {
            ++r.numPseudoMatches;
        }
    }
    return r;
}

std::ostream & operator<<(std::ostream & os, const Result & r) {
    os << "# Matches: " << r.numMatches << "\t" << "# Pseudomatches: "
              << r.numPseudoMatches << std::endl;
    return os;
}

int main() {
    srand(time(0));
    Ball solution[SLOTS];
    Ball guess[SLOTS];
    // Display possible configurations of however many matches and pseudomatches
    for (int numMatches = 0; numMatches <= SLOTS; ++numMatches) {
        for (int numPseudoMatches = 0; numPseudoMatches <= SLOTS - numMatches;
             ++numPseudoMatches) {
            while (true) {
                fillRandomly(solution);
                fillRandomly(guess);
                auto result = findResult(guess, solution);
                if (result.numMatches == numMatches &&
                    result.numPseudoMatches == numPseudoMatches) {
                    printSlots(solution);
                    printSlots(guess);
                    std::cout << result << std::endl;
                    break;
                }
            }
        }
    }
    return 0;
}
