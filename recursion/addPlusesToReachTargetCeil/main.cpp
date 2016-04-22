#include <iostream>
#include <string>
#include <cassert>

// Add pluses between digits in a string to get an expression whose value is
// as close to a target as possible without exceeding the target

void getValAux(const std::string & digits,
               const std::string::const_iterator & pos,
               std::string currNumber, int sumSoFar, const int & target,
               int & bestEverFound) {
    if (pos == digits.end()) {
        assert(!currNumber.empty());
        sumSoFar += std::stoi(currNumber);
        if (sumSoFar <= target && sumSoFar > bestEverFound) {
            bestEverFound = sumSoFar;
        }
        return;
    }
    // Add a + before the current position
    getValAux(digits, std::next(pos), std::string(1, *pos),
              sumSoFar + std::stoi(currNumber), target,
              bestEverFound);
    // Don't add a + before the current position
    getValAux(digits, std::next(pos), currNumber + *pos, sumSoFar, target,
              bestEverFound);
}

int getVal(const std::string & digits, const int & target) {
    if (digits.empty()) { return 0; }
    for (auto c : digits) {
        if (!isdigit(c)) {
            return 0;
        }
    }
    int bestEverFound = 0;
    getValAux(digits, std::next(digits.begin()),
              std::string(1, digits.front()), 0, target, bestEverFound);
    return bestEverFound;
}

int main() {
    assert(986 == getVal("19967", 1000)); // 19 + 967
    assert(95 == getVal("19967", 100)); // 19 + 9 + 67
    assert(219 == getVal("22215", 225)); // 2 + 2 + 215
    return 0;
}
