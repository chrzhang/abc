#include <iostream>
#include <cassert>

#define N 10

// Reverse a linked list

struct Node {
    int val;
    Node * next;
    Node(int v) : val(v), next(nullptr) {}
};

void swap(Node * & nptr1, Node * & nptr2) {
    auto temp = nptr1;
    nptr1 = nptr2;
    nptr2 = temp;
}

void advancePtr(Node * & p, unsigned n) {
    for (unsigned i = 0; i < n; ++i) {
         if (!p) { return; }
         p = p->next;
    }
}

struct List {
    Node * head, * tail;
    List() : head(nullptr), tail(nullptr) {}
    ~List() {
        auto curr = head;
        while (curr) {
            auto temp = curr->next;
            delete curr;
            curr = temp;
        }
    }
    void add(int val) {
        auto n = new Node(val);
        if (!head) {
            head = tail = n;
        } else {
            auto oldTail = tail;
            tail = n;
            oldTail->next = tail;
        }
    }
    void print() const {
        auto curr = head;
        while (curr) {
            std::cout << curr->val << " ";
            curr = curr->next;
        }
        std::cout << std::endl;
    }
    void reverse() {
        auto curr = head;
        Node * prev = nullptr;
        while (curr) {
            auto temp = curr->next;
            curr->next = prev;
            prev = curr;
            curr = temp;
        }
        swap(head, tail);
    }
    void reverseGroupAux(Node * pred, Node * head, Node * tail, Node * succ) {
        if (pred) { assert(pred->next == head); }
        assert(tail->next == succ);
        tail->next = nullptr; // Isolate list
        List l;
        l.head = head;
        l.tail = tail;
        l.reverse();
        if (pred) { pred->next = l.head; }
        l.tail->next = succ;
        l.head = l.tail = nullptr; // Don't destroy nodes
    }
    void reverseGroup(unsigned n) {
        if (n < 2) { return; }
        Node * p, * h, * t, * s;
        Node * np, * nh, * nt, * ns;
        p = nullptr;
        h = head;
        t = head; advancePtr(t, n - 1);
        s = nullptr;
        if (t) {
            s = t->next;
            head = t;
        }
        while (h && t) {
            np = h;
            nh = t->next;
            nt = nh; advancePtr(nt, n - 1);
            if (nt) { ns = nt->next; } else { ns = nullptr; }
            // DO WORK
            if (!t->next) {
                tail = h;
            }
            reverseGroupAux(p, h, t, s);
            p = np; h = nh; t = nt; s = ns;
        }
    }
};

int main() {
    for (int n = 0; n < N; ++n) {
        std::cout << "n = " << n << std::endl;
        for (int k = 1; k < 4; ++k) {
            std::cout << "\tk = " << k << std::endl;
            List l;
            for (int i = 0; i < n; ++i) {
                l.add(i);
            }
            l.print();
            l.reverseGroup(k);
            l.print();
            std::cout << "\n";
        }
    }
    return 0;
}
