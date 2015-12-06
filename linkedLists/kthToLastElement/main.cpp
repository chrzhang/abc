#include <iostream>

#define NUM_NODES 15

// Find the kth to last element of a singly linked list

struct Node {
    int val;
    Node * next;
    Node() {
        val = 0;
        next = nullptr;
    }
    Node(int val) {
        this->val = val;
        next = nullptr;
    }
};

struct LinkedList {
    Node * root;
    Node * tail;
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
        if (!n) return;
        if (!root) {
            root = tail = n;
        } else {
            tail->next = n;
            tail = n;
        }
        n->next = nullptr;
    }
};

std::ostream & operator<<(std::ostream & os, const LinkedList & l) {
    Node * nptr = l.root;
    while (nptr) {
        os << nptr->val << " ";
        nptr = nptr->next;
    }
    os << std::endl;
    return os;
}

const Node * findKthElemToLast(const LinkedList & l, int k) {
    // "2nd to last" is 1 before the tail so don't use 0 indexing
    if (k == 1) return l.tail;
    if (!l.root) return nullptr;
    Node * nptr1 = l.root;
    Node * nptr2 = l.root;
    for (int i = 0; i < k - 1; ++i) {
        nptr2 = nptr2->next;
        if (!nptr2) {
            return nullptr; // Asked for an invalid out-of-bounds predecessor
        }
    }
    // Walk together until the end
    while (nptr2->next) {
        nptr1 = nptr1->next;
        nptr2 = nptr2->next;
    }
    return nptr1;
}

int main() {
    int count = 1;
    LinkedList l;
    for (int i = 0; i < NUM_NODES; ++i) {
        l.append(new Node(count++));
    }
    std::cout << l << std::endl;
    for (int i = 0; i < NUM_NODES; ++i) {
        std::cout << "Finding the " << i + 1 << "th/st last element => ";
        const Node * node = findKthElemToLast(l, i + 1);
        if (node) {
            std::cout << node->val << std::endl;
        } else {
            std::cout << "INVALID k" << std::endl;
        }
    }
    return 0;
}
