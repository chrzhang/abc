#include <iostream>
#include <iomanip>
#include <ctime>
#include <cstdlib>
#include <deque>
#include <assert.h>

#define ARRAY_SIZE 700
#define NUM_ITERATIONS 100000

// Implement three stacks inside an array

struct Node {
    int val;
    Node * next;
    Node() {
        this->val = 0;
        next = nullptr;
    }
    Node(int val) {
        this->val = val;
        next = nullptr;
    }
};

struct Stack {
    Node * top, * base;
    Stack() {
        top = base = nullptr;
    }
    void push(Node * n) {
        if (!n) return;
        if (!top) {
            top = base = n;
        } else {
            Node * oldTop = top;
            n->next = oldTop;
            top = n;
        }
    }
    Node * peek() {
        return top;
    }
    Node * pop() {
        if (!top) {
            return nullptr;
        }
        Node * temp = top;
        top = top->next;
        if (!top) {
            base = nullptr;
        }
        temp->next = nullptr;
        return temp;
    }
};

std::ostream & operator<<(std::ostream & os, const Stack & s) {
    Node * nptr = s.top;
    os << "top <- ";
    while (nptr) {
        os << nptr->val << " ";
        nptr = nptr->next;
    }
    os << "<- base";
    os << std::endl;
    return os;
}

struct Array {
    Stack s1, s2, s3;
    Node * block;
    bool * freeFlagsForBlock;
    Array() {
        block = new Node[ARRAY_SIZE];
        freeFlagsForBlock = new bool[ARRAY_SIZE];
        for (int i = 0; i < ARRAY_SIZE; ++i) {
            freeFlagsForBlock[i] = true; // All free
        }
    }
    ~Array() {
        delete[] block;
        delete[] freeFlagsForBlock;
    }
    Node * getNode(int val) {
        for (int i = 0; i < ARRAY_SIZE; ++i) {
            if (freeFlagsForBlock[i]) {
                block[i].val = val;
                freeFlagsForBlock[i] = false;
                return block + i;
            }
        }
        return nullptr;
    }
    void push(int whichStack, int val) {
        switch (whichStack) {
            case 1:
                s1.push(getNode(val));
                break;
            case 2:
                s2.push(getNode(val));
                break;
            case 3:
                s3.push(getNode(val));
                break;
        }
    }
    Node * pop(int whichStack) {
        Node * n = nullptr;
        switch (whichStack) {
            case 1:
                n = s1.pop();
                break;
            case 2:
                n = s2.pop();
                break;
            case 3:
                n = s3.pop();
                break;
        }
        if (n) {
            freeFlagsForBlock[n - block] = true;
            Node * retNode = new Node(n->val);
            return retNode;
        }
        return nullptr;
    }
};

std::ostream & operator<<(std::ostream & os, const Array & a) {
    for (int i = 0; i < ARRAY_SIZE; ++i) {
        os << std::setw(14) << ((a.block)[i]).val << " ";
    }
    os << std::endl;
    for (int i = 0; i < ARRAY_SIZE; ++i) {
        std::string s = (a.freeFlagsForBlock)[i] ? "free" : "not free";
        os << std::setw(14) << s << " ";
    }
    os << std::endl;
    for (int i = 0; i < ARRAY_SIZE; ++i) {
        os << a.block + i << " ";
    }
    os << std::endl;
    return os;
}

bool matches(const std::deque<int> & s1, const Stack & s2) {
    if (!s2.top) {
        return (s1.size() == 0);
    }
    Node * nptr = s2.top;
    for (auto it = s1.begin(); it != s1.end(); ++it) {
        if (*it != nptr->val) {
            return false;
        }
        nptr = nptr->next;
    }
    assert(!nptr);
    return true;
}

int main() {
    srand(time(0));
    // Could have 3 equally sized portions of the array devoted for each stack
    // but leads to fragmentation (one stack full while others empty)
    // To be flexible, treat the array as an area of memory and allocate Nodes
    // throughout the array
    Array a;
    // Test by comparing with std::deque (instead of std::stack for easy
    // comparison assertions by iterating through std::deque)
    std::deque<int> stacks[3];
    int totalNodes = 0;
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        int whichStack = rand() % 3 + 1;
        if (totalNodes < ARRAY_SIZE) {
            // Either push or pop or do nothing
            switch (rand() % 3) {
                case 0: { // Push
                    int randomVal = rand() % 100;
                    a.push(whichStack, randomVal);
                    stacks[whichStack - 1].push_front(randomVal);
                    assert(matches(stacks[0], a.s1));
                    assert(matches(stacks[1], a.s2));
                    assert(matches(stacks[2], a.s3));
                    ++totalNodes;
                    break;
                }
                case 1: { // Pop
                    Node * n = a.pop(whichStack);
                    if (!stacks[whichStack - 1].empty()) {
                        stacks[whichStack - 1].pop_front();
                    }
                    assert(matches(stacks[0], a.s1));
                    assert(matches(stacks[1], a.s2));
                    assert(matches(stacks[2], a.s3));
                    if (n) { --totalNodes; }
                    delete n;
                    break;
                }
                case 2: { // Do nothing
                    break;
                }
            }
        } else {
            // Either pop or do nothing
            switch (rand() % 2) {
                case 0: { // Pop
                    Node * n = a.pop(whichStack);
                    if (!stacks[whichStack - 1].empty()) {
                        stacks[whichStack - 1].pop_front();
                    }
                    assert(matches(stacks[0], a.s1));
                    assert(matches(stacks[1], a.s2));
                    assert(matches(stacks[2], a.s3));
                    if (n) { --totalNodes; }
                    delete n;
                    break;
                }
                case 1: { // Do nothing
                    break;
                }
            }
        }
    }
    return 0;
}
