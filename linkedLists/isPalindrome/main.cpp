#include <iostream>
#include <ctime>
#include <cstdlib>
#include <assert.h>

#define NUM_NODES 5
#define NUM_PALINDROMES_TO_GENERATE 1000

// Detect if a linked list is a palindrome

struct Node {
    int val;
    Node * next;
    Node(int val) {
        this->val = val;
        next = nullptr;
    }
};

struct Stack {
    Node * base, * top;
    Stack() {
        base = top = nullptr;
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
        if (!base) {
            base = top = n;
        } else {
            Node * oldTop = top;
            top = n;
            top->next = oldTop;
        }
    }
    Node * peek() {
        return top;
    }
    Node * pop() {
        if (!top) {
            return nullptr;
        }
        Node * n = new Node(top->val); // Make a copy
        Node * temp = top->next;
        delete top;
        top = temp;
        if (!top) {
            base = nullptr;
        }
        return n;
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
    return os;
}

struct LinkedList {
    Node * root, * tail;
    size_t length;
    LinkedList() {
        root = tail = nullptr;
        length = 0;
    }
    ~LinkedList() {
        Node * nptr = root;
        while (nptr) {
            Node * temp = nptr->next;
            delete nptr;
            nptr = temp;
        }
    }
    void append(Node * n) {
        if (!root) {
            root = tail = n;
        } else {
            tail->next = n;
            tail = n;
        }
        ++length;
    }
    bool isPalindrome() {
        if (length <= 1) {
            return true;
        }
        Stack s; // Can be optimized by having a stack of addresses
        Node * nptr = root;
        for (int i = 0; i < length / 2; ++i) {
            s.push(new Node(nptr->val));
            nptr = nptr->next;
        }
        if (length % 2) { // Odd
            nptr = nptr->next; // Skip middle element
        }
        while (nptr) {
            Node * popped = s.pop();
            assert(popped);
            if (popped->val != nptr->val) {
                delete popped;
                return false;
            } else {
                delete popped;
            }
            nptr = nptr->next;
        }
        return true;
    }
};

std::ostream & operator<<(std::ostream & os, const LinkedList & l) {
    Node * nptr = l.root;
    os << "head <- ";
    while (nptr) {
        os << nptr->val << " ";
        nptr = nptr->next;
    }
    os << "<- tail";
    return os;
}

int main() {
    srand(time(0));
    int i = 0;
    while (i < NUM_PALINDROMES_TO_GENERATE) {
        LinkedList l;
        for (int k = 0; k < 6 + (rand() % NUM_NODES); ++k) {
            l.append(new Node(rand() % 5));
        }
        if (l.isPalindrome()) {
            std::cout << l << std::endl;
            ++i;
        }
    }
    return 0;
}
