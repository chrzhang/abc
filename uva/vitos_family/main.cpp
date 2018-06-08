#include <iostream>
#include <cassert>
#include <vector>
#include <algorithm>
#include <stdexcept>
#include <cmath>
#include <fstream>
#include <sstream>

using namespace std;

int medianOf(vector<int> values) {
    if (values.empty()) {
        throw invalid_argument("No values to find median of.");
    }
    sort(values.begin(), values.end());
    return values[values.size() / 2];
}

int sumOfDistancesFromMedian(const vector<int> & values) {
    const int median = medianOf(values);
    int result = 0;
    for (const auto & value : values) {
        result += abs(value - median);
    }
    return result;
}

int main() {
    assert(sumOfDistancesFromMedian({2, 4}) == 2);
    assert(sumOfDistancesFromMedian({2, 4, 6}) == 4);
    ifstream inFile("input.txt");
    int numLines;
    inFile >> numLines;
    string currLine;
    getline(inFile, currLine);
    for (int linei = 0; linei < numLines; ++linei) {
        getline(inFile, currLine);
        stringstream ss(currLine);
        int numCols;
        ss >> numCols;
        vector<int> values;
        for (int coli = 0; coli < numCols; ++coli) {
            int currVal;
            ss >> currVal;
            values.push_back(currVal);
        }
        cout << sumOfDistancesFromMedian(values) << endl;
    }
    return 0;
}
