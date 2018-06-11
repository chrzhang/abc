#include <iostream>
#include <cassert>
#include <algorithm>
#include <set>
#include <climits>
#include <fstream>

using namespace std;

void printSteps(const vector<vector<int>> & steps) {
    for (const auto & step : steps) {
        if (step.size() == 1) {
            cout << step[0] << endl;
        } else if (step.size() == 2) {
            cout << step[0] << " " << step[1] << endl;
        } else {
            throw invalid_argument("Step needs 1 or 2 people.");
        }
    }
}

void solveAux(multiset<int> uncrossed,
              int & lowestTotalTime,
              vector<vector<int>> & lowestTotalSteps) {
    if (uncrossed.empty()) {
        lowestTotalTime = 0;
        lowestTotalSteps = {};
        return;
    }
    if (uncrossed.size() == 1) {
        lowestTotalTime = *uncrossed.begin();
        lowestTotalSteps.push_back({*uncrossed.begin()});
        return;
    }
    // The 'obvious' approach is to have the fastest person ferry each person
    // across. However, this means the two slowest people go individually and
    // in cases where the two slowest people are much slower than everyone else,
    // this is no longer the best approach since sending them together would
    // save more time.
    // In either cases, having the fastest person on the other side after a
    // crossing return the flashlight is optimal.
    while (uncrossed.size() > 2) {
        const int fastestUncrossed = *uncrossed.begin();
        const int secondFastestUncrossed = *next(uncrossed.begin());
        const int slowestUncrossed = *uncrossed.rbegin();
        const int secondSlowestUncrossed = *(next(uncrossed.rbegin()));
        const int tToFerrySlowestIndividually =
            slowestUncrossed + fastestUncrossed + secondSlowestUncrossed + (uncrossed.size() > 3 ? fastestUncrossed : 0);
        const int tToFerrySlowestTogether = // Sending the two fastest first is necessary
            secondFastestUncrossed + fastestUncrossed + slowestUncrossed + (uncrossed.size() > 3 ? secondFastestUncrossed : 0);
        if (tToFerrySlowestIndividually <= tToFerrySlowestTogether) {
            lowestTotalSteps.push_back({fastestUncrossed, slowestUncrossed});
            lowestTotalSteps.push_back({fastestUncrossed});
            lowestTotalSteps.push_back({fastestUncrossed, secondSlowestUncrossed});
            if (uncrossed.size() > 3) {
                lowestTotalSteps.push_back({fastestUncrossed});
            } else {
                uncrossed.erase(uncrossed.begin()); // fastestUncrossed
            }
            lowestTotalTime += tToFerrySlowestIndividually;
            uncrossed.erase(prev(uncrossed.end())); // slowestUncrossed
            uncrossed.erase(prev(uncrossed.end())); // secondSlowestUncrossed
        } else {
            lowestTotalSteps.push_back({fastestUncrossed, secondFastestUncrossed});
            lowestTotalSteps.push_back({fastestUncrossed});
            lowestTotalSteps.push_back({secondSlowestUncrossed, slowestUncrossed});
            if (uncrossed.size() > 3) {
                lowestTotalSteps.push_back({secondFastestUncrossed});
            } else {
                uncrossed.erase(next(uncrossed.begin())); // secondFastestUncrossed
            }
            lowestTotalTime += tToFerrySlowestTogether;
            uncrossed.erase(prev(uncrossed.end())); // slowestUncrossed
            uncrossed.erase(prev(uncrossed.end())); // secondSlowestUncrossed
        }
    }
    if (uncrossed.size() == 2) {
        lowestTotalSteps.push_back({*uncrossed.begin(), *next(uncrossed.begin())});
        lowestTotalTime += max(*uncrossed.begin(), *next(uncrossed.begin()));
    } else if (uncrossed.size() == 1) {
        throw invalid_argument("There should never be a single person left uncrossed.");
    }
}

void solve(const vector<int> & peopleSpeeds) {
    int lowestTotalTime = 0;
    vector<vector<int>> lowestTotalSteps;
    solveAux(multiset<int>(peopleSpeeds.begin(), peopleSpeeds.end()), lowestTotalTime, lowestTotalSteps);
    cout << lowestTotalTime << endl;
    printSteps(lowestTotalSteps);
    cout << "\n";
}

int main() {
    ifstream inFile("input.txt");
    int numInputs;
    inFile >> numInputs;
    for (int inputi = 0; inputi < numInputs; ++inputi) {
        int sizeOfInput;
        inFile >> sizeOfInput;
        vector<int> speeds;
        for (int speedi = 0; speedi < sizeOfInput; ++speedi) {
            int speed;
            inFile >> speed;
            speeds.push_back(speed);
        }
        solve(speeds);
    }
}
