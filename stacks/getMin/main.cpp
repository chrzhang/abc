#include <iostream>
#include <ctime>
#include <cstdlib>
#include <limits.h>
#include <deque>
#include <algorithm>
#include <assert.h>

#define NUM_ITERATIONS 1000
#define NUM_STACK_OPS 1000

// Get the minimum element of a linked list in O(1) access

struct Node {
    int val;
    Node * next;
    Node(int val) {
        this->val = val;
        next = nullptr;
    }
};

struct Stack {
    Node * top;
    Stack() {
        top = nullptr;
    }
    ~Stack() {
        Node * nptr = top;
        while (nptr) {
            Node * temp = nptr->next;
            delete nptr;
            nptr = temp;
        }
    }
    void push(Node * n) {
        if (top) {
            n->next = top;
            top = n;
        } else {
            top = n;
        }
    }
    Node * pop() {
        if (top) {
            Node * temp = top;
            top = temp->next;
            return temp;
        }
        return top;
    }
    int peek() {
        if (!top) { return INT_MAX; }
        return top->val;
    }
};

std::ostream & operator<<(std::ostream & os, const Stack & s) {
    os << "top <- ";
    Node * nptr = s.top;
    while (nptr) {
        os << nptr->val << " ";
        nptr = nptr->next;
    }
    os << std::endl;
    return os;
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        Stack s;
        Stack minStack; // A stack with copies of minimum nodes
        std::deque<int> standardDeque;
        int currentMin = INT_MAX;
        for (int stackop = 0; stackop < NUM_STACK_OPS; ++stackop) {
            switch (rand() % 2) {
                case 0: { // Push
                    int randval = rand() % 100;;
                    s.push(new Node(randval));
                    standardDeque.push_front(randval);
                    if (currentMin >= randval) {
                        currentMin = randval;
                        minStack.push(new Node(randval));
                    }
                    assert(currentMin ==
                           *std::min_element(standardDeque.begin(),
                                             standardDeque.end()));
                    break;
                }
                case 1: { // Pop
                    Node * popped = s.pop();
                    if (popped == nullptr) { break; }
                    standardDeque.pop_front();
                    if (popped->val == minStack.top->val) {
                        delete minStack.pop();
                        currentMin = minStack.peek();
                    }
                    delete popped;
                    if (!standardDeque.empty()) {
                        assert(currentMin ==
                           *std::min_element(standardDeque.begin(),
                                             standardDeque.end()));
                    }
                    break;
                }
            }
        }
    }
    return 0;
}
