#include <iostream>
#include <cstdlib>
#include <ctime>
#include <deque>
#include <assert.h>

#define NUM_ITERATIONS 1000
#define MAX_DEQUE_OPERATIONS 1000

// Implement a deque with two stacks

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
            Node * temp = top;
            top = n;
            top->next = temp;
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
};

struct Deque {
    Stack s1, s2;
    void enqueue(Node * n) {
        // Push onto s1
        s1.push(n);
    }
    Node * dequeue() {
        // Pop everything from s1 and push them onto s2 if s2 is empty
        if (s2.isEmpty()) {
            while (!s1.isEmpty()) {
                s2.push(s1.pop());
            }
        }
        // Now return a pop from s2
        return s2.pop();
    }
};

void push_frontR(std::deque<int> & d, Node * n) {
    if (n == nullptr) { return; }
    push_frontR(d, n->next);
    d.push_front(n->val);
}

void push_backR(std::deque<int> & d, Node * n) {
    if (n == nullptr) { return; }
    push_backR(d, n->next);
    d.push_back(n->val);
}

bool matches(const Deque & d, const std::deque<int> standardDeque) {
    std::deque<int> temp;
    push_frontR(temp, d.s1.top);
    push_backR(temp, d.s2.top);
    auto it1 = temp.begin();
    auto it2 = standardDeque.begin();
    for (; it1 != temp.end(); ++it1, ++it2) {
        if (*it1 != *it2) {
            return false;
        }
    }
    if (it2 == standardDeque.end()) {
        return true;
    }
    return false;
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        Deque d;
        std::deque<int> standardDeque;
        for (int dequeop = 0; dequeop < MAX_DEQUE_OPERATIONS; ++dequeop) {
            switch(rand() % 2) {
                case 0: { // Enqueue
                    int randval = rand() % 100;
                    d.enqueue(new Node(randval));
                    standardDeque.push_front(randval);
                    assert(matches(d, standardDeque));
                    break;
                }
                case 1: { // Dequeue
                    delete d.dequeue();
                    if (!standardDeque.empty()) {
                        standardDeque.pop_back();
                    }
                    assert(matches(d, standardDeque));
                    break;
                }
            }
        }
    }
    return 0;
}
