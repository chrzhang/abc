#include <iostream>
#include <string>
#include <assert.h>
#include <ctime>
#include <cstdlib>

#define NUM_ITERATIONS 50

// Add numbers whose digits live in linked lists in increasing magnitude.

struct Node {
    char digit;
    Node * next;
    Node() {
        digit = 0;
        next = nullptr;
    }
    Node(char digit) {
        this->digit = digit;
        next = nullptr;
    }
    int getVal() {
        return digit - '0';
    }
};

struct LinkedList {
    Node * root, * tail;
    LinkedList() {
        root = tail = nullptr;
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
    }
    int toNum() {
         if (!root) {
            return 0;
        }
        std::string buffer;
        Node * nptr = root;
        while (nptr) {
            buffer.insert(0, 1, nptr->digit);
            nptr = nptr->next;
        }
        return atoi(buffer.c_str());
    }
    void fromNum(int num) {
        assert(root == nullptr);
        assert(tail == nullptr);
        std::string buffer = std::to_string(num);
        for (auto it = buffer.rbegin(); it != buffer.rend(); ++it) {
            append(new Node(*it));
        }
    }
};

std::ostream & operator<<(std::ostream & os, const LinkedList & l) {
    Node * nptr = l.root;
    while (nptr) {
        os << nptr->getVal() << " ";
        nptr = nptr->next;
    }
    return os;
}

int main() {
    srand(time(0));
    for (int k = 0; k < NUM_ITERATIONS; ++k) {
        int NUM_DIGITS1 = (rand() % 9) + 1;
        int NUM_DIGITS2 = (rand() % 9) + 1;
        LinkedList l1, l2;
        // Leading zeroes are handled like any other number for simple testing
        for (int i = 0; i < NUM_DIGITS1; ++i) {
            l1.append(new Node(rand() % 10 + '0'));
        }
        for (int i = 0; i < NUM_DIGITS2; ++i) {
            l2.append(new Node(rand() % 10 + '0'));
        }
        LinkedList lsum;
        Node * n1, * n2;
        n1 = l1.root;
        n2 = l2.root;
        int carry = 0;
        while (n1 && n2) {
            int sum = n1->getVal() + n2->getVal() + carry;
            lsum.append(new Node('0' + sum % 10));
            carry = sum / 10;
            n1 = n1->next;
            n2 = n2->next;
        }
        if (n1 || n2) { // One # was longer than the other
            Node * n = n1 ? n1 : n2;
            while (n) {
                int sum = n->getVal() + carry;
                lsum.append(new Node('0' + sum % 10));
                carry = sum / 10;
                n = n->next;
            }
        }
        if (carry) {
            lsum.append(new Node('0' + carry));
        }
        std::cout << l1.toNum() << " + " << l2.toNum() << " = " << lsum.toNum()
                  << std::endl;
        assert(l1.toNum() + l2.toNum() == lsum.toNum());
    }
    return 0;
}
