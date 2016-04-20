#include <iostream>
#include <stack>
#include <string>
#include <cassert>

// Determine if a sequence of {}, (), and [] are valid

bool isBalanced(const std::string & s) {
    std::stack<char> pStack; // Keep track of all (), {}, and []
    for (auto c : s) {
        switch (c) {
            case '(':
            case '[':
            case '{':
                pStack.push(c);
                break;
            case '}':
                if (pStack.empty() || pStack.top() != '{') {
                    return false;
                } else {
                    pStack.pop();
                }
                break;
            case ']':
                if (pStack.empty() || pStack.top() != '[') {
                    return false;
                } else {
                    pStack.pop();
                }
                break;
            case ')':
                if (pStack.empty() || pStack.top() != '(') {
                    return false;
                } else {
                    pStack.pop();
                }
                break;
            default:
                std::cout << "Invalid character " << c << " found.\n";
                return false;
        }
    }
    if (pStack.empty()) { return true; }
    else { return false; }
}

int main() {
    assert(isBalanced("(){}[()]{{}}"));
    assert(isBalanced("()"));
    assert(isBalanced("[()]"));
    assert(isBalanced("[()]{(())}[{()}]"));
    assert(!isBalanced("())"));
    assert(!isBalanced("(()"));
    assert(!isBalanced("([})"));
    assert(!isBalanced("(}{)"));
    return 0;
}
