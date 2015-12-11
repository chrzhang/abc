#include <iostream>
#include <assert.h>
#include <cstdlib>
#include <ctime>

#define MAX_STACK_LENGTH 15
#define NUM_ITERATIONS 10000

// Sort a stack in decreasing (biggest on top) order

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
        if (!n) { return; }
        if (!top) {
            top = n;
        } else {
            n->next = top;
            top = n;
        }
    }
    Node * pop() {
        if (!top) { return nullptr; }
        Node * temp = top;
        top = top->next;
        temp->next = nullptr;
        return temp;
    }
    bool isEmpty() {
        return (top == nullptr);
    }
    // By only using pushSorted, the stack will always be in a sorted state
    void pushSorted(Node * n, Stack & s) {
        if (!n) { return; }
        // Push to s as a buffer to help maintain sorting of current stack
        Node * oldTop = s.top; // Do not make any lasting changes to s
        if ((!top) || (n->val <= top->val)) {
            push(n);
            return;
        }
        while(top && n->val > top->val) {
            s.push(pop());
        }
        push(n);
        while (s.top != oldTop) {
            push(s.pop());
        }
    }
    bool isSorted() {
        if (top == nullptr) { return true; }
        Node * nptr = top;
        while (nptr) {
            if (nptr->next) {
                if (nptr->val < nptr->next->val) { return false; }
            }
            nptr = nptr->next;
        }
        return true;
    }
};

std::ostream & operator<<(std::ostream & os, const Stack & s) {
    Node * nptr = s.top;
    os << "top <- ";
    while (nptr) {
        os << nptr->val << " ";
        nptr = nptr->next;
    }
    return os;
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        int stack_length = rand() % MAX_STACK_LENGTH + 1;
        Stack s;
        Stack aux; // Allowed 1 extra stack only to help sort
        // Fill the stack with random values
        for (int nodeIndex = 0; nodeIndex < stack_length; ++nodeIndex) {
            int randval = rand() % 100;
            s.push(new Node(randval));
        }
        std::cout << s ;
        std::cout << "-> sorted -> ";
        // Begin sorting
        while (!s.isEmpty()) {
            // Fill the extra stack with stack 1 so the extra stack is sorted
            aux.pushSorted(s.pop(), s);
        }
        // aux was filled in the opposite order desired to allow easy refilling
        while (!aux.isEmpty()) {
            s.push(aux.pop());
        }
        std::cout << s << std::endl;
        assert(s.isSorted());
    }
    return 0;
}
