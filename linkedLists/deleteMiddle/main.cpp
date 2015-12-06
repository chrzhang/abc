#include <iostream>
#include <assert.h>

#define NUM_NODES 10

// Delete the middle node of a singly linked list given only that node

struct Data {
    int x, y, z;
    Data() {
        x = y = z = 0;
    }
    Data(int x, int y, int z) {
        this->x = x;
        this->y = y;
        this->z = z;
    }
    // Default copy ctor and assignment will suffice
};

struct Node {
    Data d;
    Node * next;
    Node(const Data & d) {
        this->d = d;
        this->next = nullptr;
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
        if (n == nullptr || n->next) { return; } // Do not append other lists
        if (!root) {
            root = tail = n;
        } else {
            tail->next = n;
            tail = n;
            tail->next = nullptr;
        }
    }
};

std::ostream & operator<<(std::ostream & os, const Data & d) {
    os << "{" << d.x << "," << d.y << "," << d.z << "}";
    return os;
}

std::ostream & operator<<(std::ostream & os, const LinkedList & l) {
    Node * nptr = l.root;
    while (nptr) {
        os << "[" << nptr->d << "] ";
        nptr = nptr->next;
    }
    return os;
}

void remove(Node * n) {
    // Copy n->next to n and delete n->next
    assert(n->next); // Removing the last node is an edge case w/o sentinels
    n->d = (n->next)->d;
    Node * temp = n->next;
    n->next = temp->next;
    delete temp;
}

int main() {
    // For testing, call remove on each in-between nodes one by one
    for (int k = 0; k < NUM_NODES - 1; ++k) {
        int ct = 0;
        LinkedList l;
        for (int i = 0; i < NUM_NODES; ++i) {
            Node * n = new Node(Data(ct, ct + 1, ct + 2));
            l.append(n);
            ct += 3;
        }
        std::cout << l << std::endl;
        // Find the (k + 1)th/st node
        Node * nodeToRemove = l.root;
        for (int j = 0; j < k; ++j) {
            nodeToRemove = nodeToRemove->next;
        }
        std::cout << "\tGoing to remove " << nodeToRemove-> d << std::endl;
        // Note remove is not a member function so it has limited access
        remove(nodeToRemove);
        std::cout << l << std::endl;
        std::cout << std::endl;
    }
    return 0;
}
