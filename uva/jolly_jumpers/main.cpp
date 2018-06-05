#include <iostream>
#include <cassert>
#include <vector>
#include <fstream>

using namespace std;

bool allTrue(const vector<bool> & vb) {
    for (const auto & b : vb) {
        if (!b) {
            return false;
        }
    }
    return true;
}

bool isJollyJumper(const vector<int> & seq) {
    if (seq.size() <= 1) {
        return true;
    }
    vector<bool> diffs(seq.size() - 1, false);
    auto it1 = seq.begin();
    auto it2 = next(it1);
    for (; it2 != seq.end(); ++it1, ++it2) {
        const int diff = abs(*it1 - *it2);
        diffs[diff - 1] = true;
    }
    return allTrue(diffs);
}

string solve(const vector<int> & seq) {
    return (isJollyJumper(seq) ? "Jolly" : "Not jolly");
}

int main() {
    assert("Jolly" == solve({1, 4, 2, 3}));
    assert("Jolly" == solve({-1, 1, 2}));
    assert("Not jolly" == solve({5, 1, 4, 2, -1, 6}));
    ifstream inFile("input.txt");
    int nn, ss;
    while (inFile >> nn) {
        vector<int> currSeq;
        for (int ii = 0; ii < nn; ++ii) {
            inFile >> ss;
            currSeq.push_back(ss);
        }
        cout << solve(currSeq) << endl;
    }
}
