#include <iostream>
#include <string>
#include <assert.h>
#include <ctime>
#include <cstdlib>

#define NUM_ITERATIONS 5000
#define NUM_DIGITS_MAX 9

// Add numbers whose digits live in linked lists in increasing magnitude. (1)
// Add numbers whose digits live in linked lists in decreasing magnitude. (2)

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
    size_t size;
    LinkedList() {
        root = tail = nullptr;
        size = 0;
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
        ++size;
    }
    void prepend(Node * n) {
        if (!root) {
            root = tail = n;
        } else {
            n->next = root;
            root = n;
        }
        ++size;
    }
    int toNum(bool isIncreasingMagnitude) {
         if (!root) {
            return 0;
        }
        std::string buffer;
        Node * nptr = root;
        while (nptr) {
            if (isIncreasingMagnitude) {
                buffer.insert(0, 1, nptr->digit);
            } else {
                buffer.push_back(nptr->digit);
            }
            nptr = nptr->next;
        }
        return atoi(buffer.c_str());
    }
    void fromNum(int num, bool isIncreasingMagnitude) {
        assert(root == nullptr);
        assert(tail == nullptr);
        std::string buffer = std::to_string(num);
        if (isIncreasingMagnitude) {
            for (auto it = buffer.rbegin(); it != buffer.rend(); ++it) {
                append(new Node(*it));
            }
        } else {
            for (auto it = buffer.begin(); it != buffer.end(); ++it) {
                append(new Node(*it));
            }
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

int addAux(LinkedList & lsum, Node * nptr1, Node * nptr2) {
    if (nptr1 && nptr2) {
        int carry = addAux(lsum, nptr1->next, nptr2->next);
        int sum = carry + nptr1->getVal() + nptr2->getVal();
        lsum.prepend(new Node('0' + sum % 10));
        return sum / 10;
    }
    if (!nptr1 && !nptr2) {
        return 0;
    } else {
        assert(false); // For now
    }
}

int main() {
    srand(time(0));
    std::cout << "--- Test lists when digits are in increasing magnitude\n";
    for (int k = 0; k < NUM_ITERATIONS; ++k) {
        int NUM_DIGITS1 = (rand() % NUM_DIGITS_MAX) + 1;
        int NUM_DIGITS2 = (rand() % NUM_DIGITS_MAX) + 1;
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
        std::cout << l1.toNum(1) << " + " << l2.toNum(1) << " = "
                  << lsum.toNum(1) << std::endl;
        assert(l1.toNum(1) + l2.toNum(1) == lsum.toNum(1));
    }
    std::cout << "--- Test lists when digits are in decreasing magnitude\n";
    for (int k = 0; k < NUM_ITERATIONS; ++k) {
        int NUM_DIGITS1 = (rand() % NUM_DIGITS_MAX) + 1;
        int NUM_DIGITS2 = (rand() % NUM_DIGITS_MAX) + 1;
        LinkedList l1, l2, lsum;
        for (int i = 0; i < NUM_DIGITS1; ++i) {
            l1.append(new Node(rand() % 10 + '0'));
        }
        for (int i = 0; i < NUM_DIGITS2; ++i) {
            l2.append(new Node(rand() % 10 + '0'));
        }
        // Add filler digits to generalize to the case where lists are same size
        while (l1.size < l2.size) {
            l1.prepend(new Node('0'));
        }
        while (l2.size < l1.size) {
            l2.prepend(new Node('0'));
        }
        int carry = addAux(lsum, l1.root, l2.root);
        if (carry) {
            lsum.prepend(new Node('0' + carry));
        }
        std::cout << l1.toNum(0) << " + " << l2.toNum(0) << " = "
                 << lsum.toNum(0) << std::endl;
        assert(l1.toNum(0) + l2.toNum(0) == lsum.toNum(0));
    }
    return 0;
}
