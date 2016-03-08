#include <iostream>
#include <string>
#include <vector>
#include <cassert>

// Given a string of numbers, find all possible ways of adding operands between
// the numbers to create an expression that evaluates to a desired value

int evaluate(const std::string & expr) {
    int total = 0;
    std::string s;
    char lastOp = '+';
    for (auto c : expr) {
        if (isdigit(c)) {
            s.push_back(c);
        } else {
            // Apply last op
            switch (lastOp) {
                case '+': {
                    total += std::stoi(s);
                    break;
                }
                case '-': {
                    total -= std::stoi(s);
                    break;
                }
            };
            lastOp = c;
            s.clear();
        }
    }
    switch (lastOp) {
        case '+': {
            total += std::stoi(s);
            break;
        }
        case '-': {
            total -= std::stoi(s);
            break;
        }
    };
    return total;
}

void addOperatorsAux(std::vector<std::string> & result,
                     const std::string & nums,
                     const std::string::const_iterator it,
                     std::string exprSoFar,
                     int target) {
    assert(it != nums.end());
    exprSoFar.push_back(*it);
    if (std::next(it) == nums.end()) {
        if (evaluate(exprSoFar) == target) {
            result.push_back(exprSoFar);
        }
        return;
    }
    addOperatorsAux(result, nums, next(it), exprSoFar , target);
    addOperatorsAux(result, nums, next(it), exprSoFar + "+", target);
    addOperatorsAux(result, nums, next(it), exprSoFar + "-", target);
    //addOperatorsAux(result, nums, next(it), exprSoFar + "/", target);
    //addOperatorsAux(result, nums, next(it), exprSoFar + "*", target);
}

std::vector<std::string> addOperators(std::string nums, int target) {
    std::cout << "Ways to reach " << target << " from " << nums << "\n";
    std::vector<std::string> result;
    // Recursively consider putting an operator or nothing between each
    addOperatorsAux(result, nums, nums.begin(), "", target);
    for (auto r : result) {
        std::cout << r << std::endl;
    }
    return result;
}

int main() {
    addOperators("123", 6);
    addOperators("00", 0);
    addOperators("12345", -3);
    return 0;
}
