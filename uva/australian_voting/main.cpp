#include <iostream>
#include <cassert>
#include <cmath>
#include <vector>
#include <map>
#include <algorithm>
#include <fstream>
#include <sstream>

using namespace std;

struct Ballot {
    vector<int> ranking;
    Ballot(const vector<int> & ranking)
    : ranking(ranking)
    {}
    const int getFirstPick() const {
        for (const auto & candidate : ranking) {
            if (candidate != -1) {
                return candidate;
            }
        }
        assert(0);
        return -1;
    }
};

bool allTied(const map<int, int> & voteCount) {
    if (voteCount.empty()) {
        return true;
    }
    for (const auto & vc : voteCount) {
        if (vc.second != voteCount.begin()->second) {
            return false;
        }
    }
    return true;
}

const vector<string> resolveElection(const vector<string> & candidates,
                                     vector<Ballot> ballots) {
    vector<string> winners;
    const int threshold = ceil(ballots.size() / 2.0);
    for (;;) {
        // If one candidate has majority, done
        map<int, int> voteCount;
        int lowestVoteCount = INT_MAX;
        for (const auto & ballot : ballots) {
            int & currentVoteCount = voteCount[ballot.getFirstPick()];
            currentVoteCount++;
            if (currentVoteCount >= threshold) {
                return { candidates[ballot.getFirstPick() - 1] };
            }
        }
        for (const auto & vc : voteCount) {
            lowestVoteCount = min(lowestVoteCount, vc.second);
        }
        // If all are now tied, done
        if (allTied(voteCount)) {
            for (const auto & vc : voteCount) {
                winners.push_back(candidates[vc.first - 1]);
            }
            return winners;
        }
        // Eliminate all tied for lowest votes
        auto iter = voteCount.begin();
        for (; iter != voteCount.end();) {
            if (iter->second == lowestVoteCount) {
                for (auto & ballot : ballots) { // Change ballots
                    for (auto & c : ballot.ranking) {
                        if (c == iter->first) {
                            c = -1;
                        }
                    }
                }
                iter = voteCount.erase(iter);
            } else {
                ++iter;
            }
        }
    }
    return winners;
}

vector<string> solve(const string & filename) {
    ifstream infile(filename);
    string currentLine;
    getline(infile, currentLine);
    stringstream ss(currentLine);
    int numberCandidates = -1;
    ss >> numberCandidates;
    vector<string> candidates;
    for (int ii = 0; ii < numberCandidates; ++ii) {
        string candidate;
        getline(infile, candidate);
        candidates.push_back(candidate);
    }
    string ballotDescription;
    vector<Ballot> ballots;
    while (getline(infile, ballotDescription)) {
        vector<int> ranking;
        stringstream ss(ballotDescription);
        int rank;
        while (ss >> rank) {
            ranking.push_back(rank);
        }
        ballots.push_back(Ballot(ranking));
    }
    return resolveElection(candidates, ballots);
}

int main() {
    assert(vector<string>({"Martin Van Buren"}) == solve("input1.txt"));
    assert(vector<string>({"John Doe"}) == solve("input2.txt"));
    assert(vector<string>({"Nelson Rockefeller"}) == solve("input3.txt"));
    assert(vector<string>({"Lyndon B. Johnson"}) == solve("input4.txt"));
    assert(vector<string>({"Abraham Lincoln", "Henry Wilson", "Ronald Reagan"}) == solve("input5.txt"));
    return 0;
}
