#include <iostream>
#include <vector>
#include <cassert>
#include <algorithm>
#include <climits>
#include <fstream>

using namespace std;

struct Turtle {
    int strength, weight;
    Turtle(const int strength, const int weight)
    : strength(strength), weight(weight) {}
    int leftover() const {
        return strength - weight;
    }
};

bool operator<(const Turtle & t1, const Turtle & t2) {
    if (t1.strength == t2.strength) return t1.weight < t2.weight;
    return t1.strength < t2.strength;
}

int solve(vector<Turtle> turtles) {
    // Because trying to stack the most turtles sounds like a problem with an
    // optimality in using smaller substacks, we consider DP. However, a
    // left-to-right or other directional order is not obvious from the
    // turtles.  Consider sorting the turtles by strength. Can a turtle A above
    // turtle B be stronger? First, the fact that A is above B means B has a
    // strength >= weight of both turtles. If A is even stronger than B, A can
    // be swapped with B. This quality means that a solution can be found even
    // if we sorted by strength. Now, if two turtles have the same strength,
    // then sort by weight since a lighter turtle would be able to carry more.
    // After sorting, this becomes a longest subsequence problem. Or does it?
    // Trying to iteratively find the longest stack of turtles does not
    // prove correct since skipping a really heavy turtle may be better for us
    // later on. If we find the largest stack of turtles that can fit on each
    // turtle by only considering the largest stack already found, we miss out.
    // Consider (weight strength), note it being sorted by strength:
        // 5 5 (best height is 1, itself)
        // 10 15 (best height is 2, itself and 5 5)
        // 1 21 (best height is 3, itself, 10 15, and 5 5)
        // 8 22 (best height is 2, itself and 5 5)
        // 9 23 (best height is 3, itself, 8 22, and 5 5)
    // Local optimality does not guarantee correctness.
    // The solution is 4, found by skipping the second turtle.
    // Try storing, in a 2D matrix, the minimum weight of the N-sized stack
    // considering (not necessarily using) turtles [0..j]
    // if dp[N - 1][j - 1] <= turtles[j].leftover()
    //     dp[N][j] = min(dp[N - 1][j - 1] + turtles[j].weight, dp[N][j - 1])
    // else
    //     dp[N][j] = dp[N][j - 1]
    // Meaning if the current turtle cannot support the lightest tower of
    // height - 1, the lightest tower weight considering the current turtle is
    // unchanged. Further, if the current turtle can support the lightest tower
    // of height - 1, then the weight is the smaller of (1) including the
    // current turtle into the lightest tower of height - 1 or (2) the lightest
    // tower of height excluding the current turtle.
    // We're asking is what is the smallest weight a tower of height N ? We
    // can then look to see if there are any nonzero weights from the max N to
    // 0 for our answer.
    sort(turtles.begin(), turtles.end());
    vector<vector<int>> dp(turtles.size() + 1,
                           vector<int>(turtles.size() + 1, INT_MAX));
    for (size_t i = 0; i < turtles.size(); ++i) {
        dp[0][i] = 0;
    }
    for (int n = 1; n <= (int) turtles.size(); ++n) {
        for (int j = 1; j <= (int) turtles.size(); ++j) {
            const Turtle & curr_turtle = turtles[j - 1];
            if (dp[n - 1][j - 1] <= curr_turtle.leftover()) {
                dp[n][j] = min(dp[n - 1][j - 1] + curr_turtle.weight, dp[n][j - 1]);
            } else {
                dp[n][j] = dp[n][j - 1];
            }
        }
    }
    for (int n = turtles.size(); n >= 0; --n) {
        for (int j = 0; j <= (int) turtles.size(); ++j) {
            if (dp[n][j] != INT_MAX) return n;
        }
    }
    return 0;
}

int main(int argc, const char * argv[]) {
    assert(3 == solve(vector<Turtle>({Turtle(1000, 300),
                                      Turtle(1200, 1000),
                                      Turtle(600, 200),
                                      Turtle(101, 100)})));
    if (argc != 2) {
        cerr << "Usage: " << argv[0] << " <filename>" << endl;
        return 1;
    }
    ifstream inFile(argv[1]);
    int weight, strength;
    vector<Turtle> turtles;
    while (inFile >> weight && inFile >> strength) {
        turtles.push_back(Turtle(strength, weight));
    }
    cout << solve(turtles) << endl;
}
