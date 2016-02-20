#include <iostream>

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
};

int main() {
    for (int n = 0; n < N; ++n) {
        std::cout << "n = " << n << std::endl;
        List l;
        for (int i = 0; i < n; ++i) {
            l.add(i);
        }
        l.print();
        l.reverse();
        l.print();
        std::cout << "\n";
    }
    return 0;
}
