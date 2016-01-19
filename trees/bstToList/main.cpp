#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <cassert>
#include <ctime>

#define N 10

// Convert a BST to an ordered doubly linked list

struct BinNode {
    int val;
    BinNode * n1;
    BinNode * n2;
    BinNode(int v) : val(v), n1(nullptr), n2(nullptr) {}
};

struct DLL { // Doubly linked list
    BinNode * head, * tail;
    DLL() : head(nullptr), tail(nullptr) {}
    ~DLL() {
        auto nptr = head;
        while (nptr) {
            auto temp = nptr->n2;
            delete nptr;
            nptr = temp;
        }
    }
    void push_back(BinNode * n) {
        if (!n) { return; }
        if (!head) {
            head = tail = n;
        } else {
            tail->n2 = n;
            n->n1 = tail;
            tail = n;
        }
    }
};

std::ostream & operator<<(std::ostream & os, const DLL & l) {
    os << "[ ";
    auto nptr = l.head;
    while (nptr) {
        os << nptr->val << " ";
        nptr = nptr->n2;
    }
    os << "]";
    return os;
}

struct BST { // Binary search tree
    BinNode * root;
    BST() : root(nullptr) {}
    void destroy(BinNode * n) {
        if (!n) { return; }
        destroy(n->n1);
        destroy(n->n2);
        delete n;
    }
    ~BST() {
        destroy(root);
    }
    void insertAux(BinNode * n, BinNode * parent) {
        assert(n && parent);
        if (n->val < parent->val) {
            if (parent->n1) {
                insertAux(n, parent->n1);
            } else {
                parent->n1 = n;
            }
        } else {
            if (parent->n2) {
                insertAux(n, parent->n2);
            } else {
                parent->n2 = n;
            }
        }
    }
    void insert(BinNode * n) {
        if (!n) { return; }
        if (!root) {
            root = n;
        } else {
            insertAux(n, root);
        }
    }
    void printAux(std::ostream & os, BinNode * n, int indent) const {
        if (!n) { return; }
        printAux(os, n->n2, indent + 5);
        os << std::setw(indent) << n->val << std::endl;
        printAux(os, n->n1, indent + 5);
    }
    void print(std::ostream & os) const {
        printAux(os, root, 0);
    }
    void inOrderAux(DLL & l, BinNode * n) {
        if (!n) { return; }
        inOrderAux(l, n->n1);
        l.push_back(n);
        inOrderAux(l, n->n2);
    }
    DLL inOrderConversion() {
        DLL l;
        inOrderAux(l, root);
        root = nullptr;
        return l;
    }
};

std::ostream & operator<<(std::ostream & os, const BST & t) {
    t.print(os);
    return os;
}

int main() {
    srand(time(0));
    BST t;
    for (int i = 0; i < N; ++i) {
        t.insert(new BinNode(rand() % 10));
    }
    std::cout << "Tree:\n";
    std::cout << t << std::endl;
    auto l = t.inOrderConversion();
    std::cout << "List:\n";
    std::cout << l << std::endl;
    return 0;
}
