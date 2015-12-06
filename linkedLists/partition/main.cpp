#include <iostream>
#include <ctime>
#include <cstdlib>
#include <assert.h>

#define NUM_NODES 10
#define NUM_ITERATIONS 5

// Partition a linked list so all nodes with value < pivot come before those >

struct Node {
    int val;
    Node * next;
    Node(int val) {
        this->val = val;
        next = nullptr;
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
        if (n == nullptr || n->next) { return; } // Only work on lone nodes
        if (!root) {
            root = tail = n;
        } else {
            tail->next = n;
            tail = n;
        }
    }
    void prepend(Node * n) {
        if (n == nullptr || n->next) { return; }
        if (!root) {
            root = tail = n;
        } else if (root == n) {
            return;
        } else {
            Node * oldRoot = root;
            root = n;
            root->next = oldRoot;
        }
    }
};

std::ostream & operator<<(std::ostream & os, const LinkedList & l) {
    Node * nptr = l.root;
    while (nptr) {
        os << nptr->val << " ";
        nptr = nptr->next;
    }
    return os;
}

int main() {
    srand(time(0));
    for (int i = 0; i < NUM_ITERATIONS; ++i) {
        LinkedList l;
        for (int j = 0; j < NUM_NODES; ++j) {
            l.append(new Node(rand() % 10));
        }
        std::cout << l << std::endl;
        int pivot = rand() % 10;
        std::cout << "\tPivot = " << pivot << std::endl;
        Node * prev, * n;
        prev = nullptr;
        n = l.root;
        for (int i = 0; i < NUM_NODES; ++i) {
            if (l.root == n) {
                l.root = n->next;
            } else {
                prev->next = n->next;
                if (!prev->next) {
                    l.tail = prev;
                }
            }
            Node * temp = n->next;
            n->next = nullptr; // Cut off node
            if (n->val <= pivot) {
                l.prepend(n);
            } else {
                l.append(n);
            }
            n = temp;
            if (prev == nullptr) {
                prev = l.root;
            } else if (prev->next == n) {
                // Don't change prev
            } else {
                prev = prev->next;
            }
        }
        std::cout << "Partitioned: " << l << std::endl;
        std::cout << std::endl;
    }
    return 0;
}
