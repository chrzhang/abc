class Solution {
public:
    int evalRPN(vector<string>& tokens) {
        stack<string> myStack;
        for (auto token : tokens) {
            if (token == "+") {
                auto s1 = myStack.top();
                myStack.pop();
                auto s2 = myStack.top();
                myStack.pop();
                myStack.push(to_string(stoi(s1) + stoi(s2)));
            } else if (token == "-") {
                auto s1 = myStack.top();
                myStack.pop();
                auto s2 = myStack.top();
                myStack.pop();
                myStack.push(to_string(stoi(s2) - stoi(s1)));
            } else if (token == "*") {
                auto s1 = myStack.top();
                myStack.pop();
                auto s2 = myStack.top();
                myStack.pop();
                myStack.push(to_string(stoi(s1) * stoi(s2)));
            } else if (token == "/") {
                auto s1 = myStack.top();
                myStack.pop();
                auto s2 = myStack.top();
                myStack.pop();
                myStack.push(to_string(stoi(s2) / stoi(s1)));
            } else {
                myStack.push(token);
            }
        }
        assert(myStack.size() == 1);
        return stoi(myStack.top());
    }
};