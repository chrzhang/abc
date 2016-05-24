#include <iostream>
#include <cassert>
#include <string>
#include <vector>
#include <stdlib.h>
#include <sstream>
#include <algorithm>

// Given players' names and scores as well as winnings for each winning place,
// calculate the earnings each player earns

double toDouble(const std::string & s) {
    std::istringstream i(s);
    double x;
    char c;
    if (!(i >> x) || i.get(c)) {
        assert(0);
    }
    return x;
}

struct PlayerEntry {
    std::string name;
    double score;
    int earnings;
    int originalRank;
    int sortedRank;
    PlayerEntry(const std::string & str, int orank) {
        auto seek = str.find(':');
        if (seek == std::string::npos) { assert(0); }
        name = std::string(str, 0, seek);
        score = toDouble(std::string(str, seek + 2));
        earnings = 0;
        originalRank = orank;
    }
};

std::ostream & operator<<(std::ostream & os, const PlayerEntry & pe) {
    os << pe.name << ": " << pe.sortedRank << ", " << pe.earnings;
    return os;
}

std::vector<std::string> calcEarnings(const std::vector<std::string> & scores,
                                      const std::vector<int> & money) {
    // "Name: Score" entries are assumed valid
    std::vector<std::string> result;
    std::vector<PlayerEntry> playerEntries;
    int i = 0;
    for (auto s : scores) { // Convert input from raw strings
        playerEntries.push_back(PlayerEntry(s, i++));
    }
    std::sort(playerEntries.begin(), playerEntries.end(),
              [](const PlayerEntry & pe1, const PlayerEntry & pe2) {
                    return pe1.score > pe2.score; });
    for (int i = 0; i < (int) playerEntries.size(); ++i) {
        if (i == 0 || playerEntries[i].score != playerEntries[i - 1].score) {
            playerEntries[i].sortedRank = i + 1; // Ranks not 0-based
        } else {
            playerEntries[i].sortedRank = playerEntries[i - 1].sortedRank;
        }
    }
    int currPlace = 0; // First place
    for (auto it = playerEntries.begin(); it != playerEntries.end();) {
        auto after = std::next(it);
        // If there are three ties for 2nd place, the earnings of 2nd, 3rd, and
        // 4th place are summed and split evenly across 3 players
        int numContenders = 1;
        if (it->score <= 0) { break; } // Do not reward non-pos scores
        while (after != playerEntries.end() && after->score == it->score) {
            ++numContenders;
            ++after;
        }
        int amt = 0;
        for (int i = 0; i < numContenders; ++i) {
            if (currPlace + i < (int) money.size()) {
                amt += money[currPlace + i];
            }
        }
        amt /= numContenders;
        for (auto k = it; k != after; ++k) {
            k->earnings = amt;
        }
        currPlace += numContenders;
        it = after;
    }
    // Present results in original order
    std::sort(playerEntries.begin(), playerEntries.end(),
              [](const PlayerEntry & pe1, const PlayerEntry & pe2) {
                    return pe1.originalRank < pe2.originalRank; });
    for (auto pe : playerEntries) {
        std::ostringstream stream;
        stream << pe;
        result.push_back(stream.str());
    }
    return result;
}

int main() {
    {
        std::vector<std::string> scores = {
            "ads: 550.34",
            "talub: 2102.98",
            "romana: 1123.21",
            "mike: -1000.00",
            "td: 1123.21",
            "dok: 1123.21",
            "dwarfsleepy: 812.12" };
        std::vector<int> money = { 300, 150, 75 };
        std::vector<std::string> result = calcEarnings(scores, money);
        std::vector<std::string> correctResult = { "ads: 6, 0",
                                                   "talub: 1, 300",
                                                   "romana: 2, 75",
                                                   "mike: 7, 0",
                                                   "td: 2, 75",
                                                   "dok: 2, 75",
                                                   "dwarfsleepy: 5, 0" };

        assert(result == correctResult);
    }
    {
        std::vector<std::string> scores = {
            "buddy: 100.00",
            "carl: 0.00" };
        std::vector<int> money = { 10, 20, 30};
        std::vector<std::string> result = calcEarnings(scores, money);
        std::vector<std::string> correctResult = { "buddy: 1, 10",
                                                   "carl: 2, 0" };
        assert(result == correctResult);
    }
    {
        std::vector<std::string> scores = {
            "buddy: 100.00",
            "carl: 0.00" };
        std::vector<int> money = { 10, 20, 30};
        std::vector<std::string> result = calcEarnings(scores, money);
        std::vector<std::string> correctResult = { "buddy: 1, 10",
                                                   "carl: 2, 0" };
        assert(result == correctResult);
    }
    {
        std::vector<std::string> scores = {
            "a: 100.00",
            "b: 100.00",
            "c: 100.00",
            "d: 100.00",
            "e: 100.00",
            "f: 100.00",
            "g: 100.00" };
        std::vector<int> money = { 1000, 500};
        std::vector<std::string> result = calcEarnings(scores, money);
        std::vector<std::string> correctResult = { "a: 1, 214", // 1500 / 7
                                                   "b: 1, 214",
                                                   "c: 1, 214",
                                                   "d: 1, 214",
                                                   "e: 1, 214",
                                                   "f: 1, 214",
                                                   "g: 1, 214" };
        assert(result == correctResult);
    }
    return 0;
}
