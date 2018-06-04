#include <iostream>
#include <cassert>

using namespace std;

bool whoWins(const int p) {
    bool currPlayer = 0;
    int currRangeStart = 1;
    int currRangeEnd = 9;
    while (p > currRangeEnd) {
        currRangeStart = currRangeEnd + 1;
        currPlayer = !currPlayer;
        if (currPlayer) {
            currRangeEnd *= 2;
        } else {
            currRangeEnd *= 9;
        }
    }
    return currPlayer;
}

int main() {
    // Playing perfect in this sense means minimizing the opportunities for the opponent to win
    // 1 < p <= 9 : Stan because he'd just pick 9
    // 9 < p <= 2 * 9 : Ollie because he'd pick 9 but the lowest Stan could have picked is 2
    // He's where logic for the upper bound starts to repeat
    // 2 * 9 < p <= 2 * 9 * 9 : Stan because he'd pick 9
    // 2 * 9 * 9 < p <= 2 * 2 * 9 * 9 : Ollie because he'd pick 9 but the lowest Stan could have picked is 2
    const bool STAN = 0;
    const bool OLLIE = 1;
    for (int p = 2; p <= 9; ++p) {
        assert(STAN == whoWins(p));
    }
    for (int p = 10; p <= 18; ++p) {
        assert(OLLIE == whoWins(p));
    }
    for (int p = 19; p <= 162; ++p) {
        assert(STAN == whoWins(p));
    }
    for (int p = 163; p <= 324; ++p) {
        assert(OLLIE == whoWins(p));
    }
}
