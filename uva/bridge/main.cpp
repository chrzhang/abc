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

void solveAux(const multiset<int> uncrossed, const multiset<int> crossed,
              const int totalTime, int & lowestTotalTime,
              const vector<vector<int>> & steps,
              vector<vector<int>> & lowestTotalSteps) {
    if (totalTime > lowestTotalTime) {
        return;
    }
    if (uncrossed.empty()) {
        if (totalTime < lowestTotalTime) {
            lowestTotalTime = totalTime;
            lowestTotalSteps = steps;
        }
        return;
    }
    if (uncrossed.size() == 1) {
        throw invalid_argument("");
    }
    // The 'obvious' approach is to have the fastest person ferry each person
    // across. However, this means the two slowest people go individually and
    // in cases where the two slowest people are much slower than everyone else,
    // this is no longer the best approach since sending them together would
    // save more time.
    // In either cases, having the fastest person on the other side after a
    // crossing return the flashlight is optimal
    // Entertain both strategies
    const bool peopleAlreadyCrossed = !crossed.empty();
    const int fastestCrossed = *crossed.begin();
    const int fastestUncrossed = *uncrossed.begin();
    const int secondFastestUncrossed = *next(uncrossed.begin());
    const int personSentBackIf2FastestWent =
        (peopleAlreadyCrossed ? min(fastestCrossed, fastestUncrossed) : fastestUncrossed);
    int tFor2Fastest =
        max(fastestUncrossed, secondFastestUncrossed) + personSentBackIf2FastestWent;
    const int slowestUncrossed = *uncrossed.rbegin();
    const int secondSlowestUncrossed = *next(uncrossed.rbegin());
    const int personSentBackIf2SlowestWent =
        (peopleAlreadyCrossed ? min(*crossed.begin(), secondSlowestUncrossed) : secondSlowestUncrossed);
    int tFor2Slowest =
        max(slowestUncrossed, secondSlowestUncrossed) + personSentBackIf2SlowestWent;
    { // Send the two fastest
        auto fast_steps = steps;
        fast_steps.push_back({fastestUncrossed, secondFastestUncrossed});
        auto fast_uncrossed = uncrossed;
        fast_uncrossed.erase(fast_uncrossed.begin());
        fast_uncrossed.erase(fast_uncrossed.begin());
        auto fast_crossed = crossed;
        fast_crossed.emplace(fastestUncrossed);
        fast_crossed.emplace(secondFastestUncrossed);
        if (!fast_uncrossed.empty()) {
            fast_steps.push_back({personSentBackIf2FastestWent});
            fast_crossed.erase(personSentBackIf2FastestWent);
            fast_uncrossed.emplace(personSentBackIf2FastestWent);
        } else {
            tFor2Fastest -= personSentBackIf2FastestWent;
        }
        solveAux(fast_uncrossed, fast_crossed, totalTime + tFor2Fastest, lowestTotalTime, fast_steps, lowestTotalSteps);
    }
    { // Send the two slowest
        auto slow_steps = steps;
        slow_steps.push_back({secondSlowestUncrossed, slowestUncrossed});
        auto slow_uncrossed = uncrossed;
        slow_uncrossed.erase(prev(slow_uncrossed.end()));
        slow_uncrossed.erase(prev(slow_uncrossed.end()));
        auto slow_crossed = crossed;
        slow_crossed.emplace(secondSlowestUncrossed);
        slow_crossed.emplace(slowestUncrossed);
        if (!slow_uncrossed.empty()) {
            slow_steps.push_back({personSentBackIf2SlowestWent});
            slow_crossed.erase(personSentBackIf2SlowestWent);
            slow_uncrossed.emplace(personSentBackIf2SlowestWent);
        } else {
            tFor2Slowest -= personSentBackIf2SlowestWent;
        }
        solveAux(slow_uncrossed, slow_crossed, totalTime + tFor2Slowest, lowestTotalTime, slow_steps, lowestTotalSteps);
    }
}

void solve(const vector<int> & peopleSpeeds) {
    if (peopleSpeeds.size() == 1) {
        cout << *peopleSpeeds.begin() << endl;
        return;
    }
    int lowestTotalTime = INT_MAX;
    vector<vector<int>> lowestTotalSteps;
    solveAux(multiset<int>(peopleSpeeds.begin(), peopleSpeeds.end()), {}, 0, lowestTotalTime, {}, lowestTotalSteps);
    cout << lowestTotalTime << endl;
    printSteps(lowestTotalSteps);
}

int main() {
    solve({1, 2, 5, 10});
}
