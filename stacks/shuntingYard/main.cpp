#include <iostream>
#include <string>
#include <stack>
#include <cassert>

// Convert an infix expression into postfix using Dijkstra's shunting yard
// algorithm

bool isOperator(char c) {
    return (c == '+' || c == '-' || c == '*' || c == '/');
}

bool isOperand(char c) {
    return !isOperator(c);
}

bool hasLowerOrEqualAndLeftPrec(char c1, char c2) {
    if ((c1 == '+' || c1 == '-') && (c2 == '+' || c2 == '-')) {
        return true;
    }
    if ((c1 == '*' || c1 == '/') && (c2 == '*' || c2 == '/')) {
        return true;
    }
    if ((c1 == '+' || c1 == '-') && (c2 == '*' || c2 == '/')) {
        return true;
    }
    return false;
}

std::string toPostfix(const std::string & expr) {
    std::cout << "Converting " << expr;
    std::string result;
    std::stack<char> stack;
    for (auto c : expr) {
        if (isOperand(c)) {
            result.push_back(c);
        } else if (isOperator(c)) {
            if (stack.empty()) {
                stack.push(c);
            } else {
                if (hasLowerOrEqualAndLeftPrec(c, stack.top())) {
                    for (;;) {
                        result.push_back(stack.top());
                        stack.pop();
                        if (stack.empty() ||
                            !hasLowerOrEqualAndLeftPrec(c, stack.top())) {
                            break;
                        }
                    }
                    stack.push(c);
                } else {
                    stack.push(c);
                }
            }
        } else {
            std::cout << "Unsupported character " << c << "\n";
            return "";
        }
    }
    while (!stack.empty()) {
        result.push_back(stack.top());
        stack.pop();
    }
    std::cout << " ==> " << result << "\n";
    return result;
}

int main() {
    assert("AB*C+" == toPostfix("A*B+C"));
    assert("ABC*+" == toPostfix("A+B*C"));
    assert("AB-C+" == toPostfix("A-B+C"));
    return 0;
}
