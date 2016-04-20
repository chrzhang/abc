#include <iostream>
#include <vector>
#include <assert.h>

#define NUM_PAIRS 5

// Build all valid arrangements of N pairs of ()

enum Paren { opening, closing };

std::ostream & operator<<(std::ostream & os, const Paren & p) {
    if (p == opening) {
        os << "(";
    } else {
        os << ")";
    }
    return os;
}

// Recursively add opening and closing whenever possible
void genAux(std::vector<std::vector<Paren>> & arrangements,
            std::vector<Paren> currentArrangement, int currBalance,
            int numOpening, int numClosing) {
    assert(currBalance >= 0);
    if (numOpening == 0 && numClosing == 0) {
        arrangements.push_back(currentArrangement);
        return;
    }
    auto copyOfCurrentArrangement = currentArrangement;
    if (numOpening > 0) { // Add an opening (
        currentArrangement.push_back(opening);
        genAux(arrangements, currentArrangement, currBalance + 1,
               numOpening - 1, numClosing);
    }
    if (numClosing > 0 && currBalance > 0) { // Add a closing )
        copyOfCurrentArrangement.push_back(closing);
        genAux(arrangements, copyOfCurrentArrangement, currBalance - 1,
               numOpening, numClosing - 1);
    }
    return;
}

std::vector<std::vector<Paren>> genArrangements(int numPairs) {
    std::vector<std::vector<Paren>> arrangements;
    std::vector<Paren> currentArrangement;
    genAux(arrangements, currentArrangement, 0, numPairs, numPairs);
    return arrangements;
}

int main() {
    for (int n = 0; n <= NUM_PAIRS; ++n) {
        std::cout << "Arrangements of " << n << " pairs:\n";
        auto arrangements = genArrangements(n);
        for (auto it1 = arrangements.begin(); it1 != arrangements.end();
                                              ++it1) {
            std::cout << "\t";
            for (auto it2 = it1->begin(); it2 != it1->end(); ++it2) {
                std::cout << *it2;
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
