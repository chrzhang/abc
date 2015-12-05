#include <iostream>
#include <ctime>
#include <cstdlib>
#include <unordered_map>

#define NUM_NODES 10

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
    }
}

int main() {
    srand(time(0));
    LinkedList * l = new LinkedList;
    for (int i = 0; i < NUM_NODES; ++i) {
        Node * n = new Node(rand() % 5);
        l->append(n);
    }
    std::cout << *l;
    std::cout << "Removing duplicates." << std::endl;
    removeDuplicates(*l);
    std::cout << *l;
    delete l;
    return 0;
}
