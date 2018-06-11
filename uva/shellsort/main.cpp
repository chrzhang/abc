#include <iostream>
#include <vector>
#include <cassert>
#include <fstream>

using namespace std;

int shellSortAux(const vector<int> & unsortedIndices, const int amountIgnored) {
    // It is OK for amountIgnored from the top to be out of order
    for (int ii = unsortedIndices.size() - 1, orderi = ii; ii >= 0; --ii, --orderi) {
        if (orderi != unsortedIndices[ii]) { // Out of order
            if (unsortedIndices[ii] < amountIgnored) { // But OK
                orderi += 1;
            } else {
                return shellSortAux(unsortedIndices, amountIgnored + 1);
            }
        }
    }
    return amountIgnored; // Reach here if nothing was out of order and not OK
}

void shellSort(const vector<string> & unsorted, const vector<string> & sorted) {
    assert(unsorted.size() == sorted.size());
    if (unsorted.empty()) return;
    vector<int> unsortedIndices;
    for (const auto & turtle : unsorted) {
        bool foundInSorted = false;
        for (size_t ii = 0; ii < sorted.size(); ++ii) {
            if (sorted[ii] == turtle) {
                unsortedIndices.push_back(ii);
                foundInSorted = true;
                break;
            }
        }
        assert(foundInSorted);
    }
    const int bound = shellSortAux(unsortedIndices, 0);
    for (int ii = bound - 1; ii >= 0; --ii) {
        cout << sorted[ii] << endl;
    }
    cout << endl;
}

int main() {
    /* The order of the turtles commanded to crawl to the top will be a suffix
     * of [N, N -1, ... 1] since that is the only way the result will be in
     * ascending order. The problem then reduces to finding the shortest suffix
     * possible. Because the suffix must be consecutive, the only real question
     * that remains is finding the first element of the suffix. */
    ifstream inputFile("input.txt");
    int numInputs;
    inputFile >> numInputs;
    for (int inputIndex = 0; inputIndex < numInputs; ++inputIndex) {
        int sizeOfStack;
        inputFile >> sizeOfStack;
        string turtle;
        getline(inputFile, turtle);
        vector<string> unsorted;
        for (int turtleIndex = 0; turtleIndex < sizeOfStack; ++turtleIndex) {
            getline(inputFile, turtle);
            unsorted.push_back(turtle);
        }
        vector<string> sorted;
        for (int turtleIndex = 0; turtleIndex < sizeOfStack; ++turtleIndex) {
            getline(inputFile, turtle);
            sorted.push_back(turtle);
        }
        assert(sorted.size() == unsorted.size());
        shellSort(unsorted, sorted);
    }
    return 0;
}
