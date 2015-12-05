#include <iostream>
#include <ctime>
#include <cstdlib>
#include <unordered_map>
#include <assert.h>

#define NUM_NODES 50

// Remove duplicates from an unsorted linked list.

struct Node {
    int value;
    Node * next;
    Node() {
        value = 0;
        next = nullptr;
    }
    Node(int value) {
        this->value = value;
        next = nullptr;
    }
};

struct Found {
    bool discovered;
    Found() { discovered = false; }
};

struct LinkedList {
    Node * root;
    Node * tail;
    LinkedList() {
        root = tail = nullptr;
    }
    ~LinkedList() {
        Node * nptr = root;
        while (nptr != nullptr) {
            Node * temp = nptr;
            nptr = nptr->next;
            delete temp;
        }
    }
    void append(Node * n) {
        if (root == nullptr) {
            root = n;
            tail = n;
        } else {
            tail->next = n;
            tail = n;
        }
    }
};

std::ostream & operator<<(std::ostream & os, const LinkedList & l) {
    Node * nptr = l.root;
    os << "root -> ";
    while (nptr != nullptr) {
        os << "[" << nptr->value << "] ";
        nptr = nptr->next;
    }
    os << "<- tail" << std::endl;
    return os;
}

void removeDuplicates(LinkedList & l) {
    std::unordered_map<int, Found> ht; // Store whatever is encountered
    Node * nptr = l.root;
    Node * prev = nullptr;
    while (nptr != nullptr) {
        if (ht[nptr->value].discovered) {
            prev->next = nptr->next;
            delete nptr;
            nptr = prev->next;
        } else {
            ht[nptr->value].discovered = true;
            nptr = nptr->next;
            prev = (prev == nullptr ? l.root : prev->next);
        }
        l.tail = nptr;
    }
}

void slowRemoveDuplicates(LinkedList & l) {
    // No auxiliary storage
    Node * prev1 = l.root;
    Node * nptr1 = (l.root)->next;
    while (prev1 && nptr1) {
        Node * prev2 = prev1;
        Node * nptr2 = nptr1;
        while (prev2 && nptr2) {
            if (nptr2->value == prev1->value) {
                prev2->next = nptr2->next;
                delete nptr2;
                nptr2 = prev2;
            } else {
                prev2 = prev2->next;
            }
            nptr2 = nptr2->next;
        }
        if (prev1->next) {
            prev1 = prev1->next;
            nptr1 = prev1->next;
        } else {
            l.tail = prev1;
            break;
        }
    }
}

int main() {
    srand(time(0));
    LinkedList l;
    for (int i = 0; i < NUM_NODES; ++i) {
        Node * n = new Node(rand() % 5);
        l.append(n);
    }
    std::cout << l;
    std::cout << "Removing duplicates." << std::endl;
    slowRemoveDuplicates(l);
    std::cout << l;
    return 0;
}
