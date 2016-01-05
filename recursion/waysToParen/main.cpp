#include <iostream>
#include <string>
#include <assert.h>
#include <map> // For DP

// Count ways to parenthesize a boolean expression to achieve a given result

bool isOperand(const char c) {
    switch (c) {
        case '|':
        case '^':
        case '&':
            return true;
        default:
            return false;
    }
}

bool isValidBoolExpr(const std::string & expr) {
    for (auto it = expr.begin(); it != expr.end(); ++it) {
        switch (*it) {
            case '1':
            case '0':
            case '|':
            case '&':
            case '^':
                break;
            default:
                return false;
        }
    }
    return true;
}

struct PriorResult {
    std::string expr;
    bool result;
    PriorResult(std::string e, bool r) : expr(e), result(r) {}
};

bool operator==(const PriorResult & pr1, const PriorResult & pr2) {
    if (pr1.expr.compare(pr2.expr) != 0) {
        return false;
    }
    return pr1.result == pr2.result;
}

bool operator<(const PriorResult & pr1, const PriorResult & pr2) {
    if (pr1.expr.compare(pr2.expr) == 0) {
        return pr1.result < pr2.result;
    } else {
        return pr1.expr.compare(pr2.expr) < 0;
    }
}

size_t countWays(const std::string & expr, const bool result,
                 std::map<PriorResult, size_t> & table) {
    auto seekPriorCalculation = table.find(PriorResult(expr, result));
    if (seekPriorCalculation != table.end()) {
        return seekPriorCalculation->second;
    }
    assert(isValidBoolExpr(expr));
    if (expr.size() == 1) { // Base case
        if (expr.compare("1") == 0) {
            table[PriorResult(expr, result)] = result == true;
            return (result == true);
        } else {
            table[PriorResult(expr, result)] = result == false;
            return (result == false);
        }
    }
    size_t ways = 0;
    // Go through and consider each operand as a delimiter of two halves
    for (auto it = expr.begin(); it != expr.end(); ++it) {
        if (isOperand(*it)) {
            auto left = std::string(expr.begin(), it);
            auto right = std::string(std::next(it, 1), expr.end());
            // Count ways to make the left and right side so the operand
            // achieves the desired result
            switch (*it) {
                case '|': {
                    if (result == true) { // Either left, right, or both true
                        ways += countWays(left, true, table) *
                                    countWays(right, true, table) +
                               countWays(left, false, table) *
                                    countWays(right, true, table) +
                               countWays(left, true, table) *
                                    countWays(right, false, table);
                    } else { // Only when both false
                        ways += countWays(left, false, table) *
                                countWays(right, false, table);
                    }
                    break;
                }
                case '&': {
                    if (result == true) { // Only when both true
                        ways += countWays(left, true, table) *
                                countWays(right, true, table);
                    } else { // Either left, right, or both false
                        ways +=
                            countWays(left, false, table) *
                                countWays(right, true, table) +
                            countWays(left, true, table) *
                                countWays(right, false, table) +
                            countWays(left, false, table) *
                                countWays(right, false, table);
                    }
                    break;
                }
                case '^': {
                    if (result == true) { // Both differ
                        ways +=
                            countWays(left, true, table) *
                                countWays(right, false, table) +
                            countWays(left, false, table) *
                                countWays(right, true, table);
                    } else { // Both match
                        ways +=
                            countWays(left, true, table) *
                                countWays(right, true, table) +
                            countWays(left, false, table) *
                                countWays(right, false, table);
                    }
                    break;
                }
            }
        }
    }
    table[PriorResult(expr, result)] = ways;
    return ways;
}

int main() {
    std::string s = "1^0|0|1";
    assert(isValidBoolExpr(s));
    // unordered_map requires hash to be defined for custom struct keys
    std::map<PriorResult, size_t> table;
    std::cout << s << " has " << countWays(s, false, table)
              << " ways of parenthesizing to false." << std::endl;
    assert(2 == countWays(s, false, table)); // 1^((0|0)|1) and 1^(0|(0|1))
    return 0;
}
