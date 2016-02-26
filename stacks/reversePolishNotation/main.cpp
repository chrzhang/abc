#include <iostream>
#include <stack>
#include <string>
#include <vector>
#include <cassert>

// Evaluate a reverse Polish notation expression

int evalRPN(std::vector<std::string> & tokens) {
    std::stack<std::string> myStack;
    for (auto token : tokens) {
        if (token == "+") {
            auto s1 = myStack.top();
            myStack.pop();
            auto s2 = myStack.top();
            myStack.pop();
            myStack.push(std::to_string(std::stoi(s1) + std::stoi(s2)));
        } else if (token == "-") {
            auto s1 = myStack.top();
            myStack.pop();
            auto s2 = myStack.top();
            myStack.pop();
            myStack.push(std::to_string(std::stoi(s2) - std::stoi(s1)));
        } else if (token == "*") {
            auto s1 = myStack.top();
            myStack.pop();
            auto s2 = myStack.top();
            myStack.pop();
            myStack.push(std::to_string(std::stoi(s1) * std::stoi(s2)));
        } else if (token == "/") {
            auto s1 = myStack.top();
            myStack.pop();
            auto s2 = myStack.top();
            myStack.pop();
            myStack.push(std::to_string(std::stoi(s2) / std::stoi(s1)));
        } else {
            myStack.push(token);
        }

    }
    assert(myStack.size() == 1);
    return std::stoi(myStack.top());
}

int main() {
    std::vector<std::string> tokens;
    tokens = { "2", "1", "+", "3", "*" };
    assert(evalRPN(tokens) == 9);
    tokens = { "4", "13", "5", "/", "+" };
    assert(evalRPN(tokens) == 6);
    return 0;
}
