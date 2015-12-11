#include <iostream>

#define MAX_NUM_DISCS 10

// Solve the Towers of Hanoi problem using stacks

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
};

std::ostream & operator<<(std::ostream & os, const Stack & s) {
    os << "top <- ";
    for (Node * nptr = s.top; nptr != nullptr; nptr = nptr->next) {
        os << nptr->val << " ";
    }
    os << std::endl;
    return os;
}

void move(int numDiscs, Stack & src, Stack & dest, Stack & buffer) {
    if (1 == numDiscs) {
        dest.push(src.pop());
        return;
    }
    move(numDiscs - 1, src, buffer, dest);
    move(1, src, dest, buffer);
    move(numDiscs - 1, buffer, dest, src);
}

int main() {
    // There are 3 towers and N discs that start of in increasing order on
    // tower 1. We would like to see these towers in increasing order on tower
    // 3, following the rules of the puzzle. The problem for N >= 2 is to move
    // the first N-1 discs onto tower 2, using tower 3 as a "buffer", then to
    // move the last disc on tower 1 to tower 3, and finally moving all the N-1
    // discs on tower 2 to tower 3.
    for(int numDiscs = 1; numDiscs <= MAX_NUM_DISCS; ++numDiscs) {
        Stack tower1, tower2, tower3;
        for (int disc = 0; disc < numDiscs; ++disc) {
            tower1.push(new Node(numDiscs - disc));
        }
        std::cout << "Problem: " << std::endl;
        std::cout << "\ttower1 | " << tower1;
        std::cout << "\ttower2 | " << tower2;
        std::cout << "\ttower3 | " << tower3;
        move(numDiscs, tower1, tower3, tower2);
        std::cout << "Solved: " << std::endl;
        std::cout << "\ttower1 | " << tower1;
        std::cout << "\ttower2 | " << tower2;
        std::cout << "\ttower3 | " << tower3;
        std::cout << std::endl;
    }
    return 0;
}
