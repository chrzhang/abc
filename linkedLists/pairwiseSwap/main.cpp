#include <iostream>
#include <cstdlib>
#include <ctime>

#define N 10

// Swap every pair of nodes in a list

struct Node {
    int val;
    Node * next;
    Node(int v) : val(v), next(nullptr) {}
};

struct List {
    Node * head, * tail;
    List() : head(nullptr), tail(nullptr) {}
    ~List() {
        Node * curr = head;
        while (curr) {
            Node * temp = curr->next;
            delete curr;
            curr = temp;
        }
    }
    void add(int val) {
        Node * n = new Node(val);
        if (!head) {
            head = tail = n;
        } else {
            Node * oldTail = tail;
            tail = n;
            oldTail->next = tail;
        }
    }
    void print() const {
        Node * curr = head;
        std::cout << "\t";
        while (curr) {
            std::cout << curr->val << " ";
            curr = curr->next;
        }
        std::cout << std::endl;
    }
    void swapEveryPair() {
        Node * pred, * first, * second;
        pred = nullptr;
        first = head;
        if (first) {
            second = head->next;
        }
        if (!(first && second)) { return; }
        while (first && second) {
            if (!pred) {
                head = second;
            } else {
                pred->next = second;
            }
            Node * succ = second->next;
            second->next = first;
            first->next = succ;
            pred = first;
            first = first->next;
            if (first) {
                second = first->next;
            } else {
                break;
            }
        }
        pred->next = first;
        tail = first ? pred : first;
    }
};

int main() {
    srand(time(0));
    for (size_t n = 1; n < N; ++n) {
        std::cout << "n = " << n << std::endl;
        List l;
        for (int i = 0; i < n; ++i) {
            l.add(i);
        }
        l.print();
        l.swapEveryPair();
        l.print();
    }
    return 0;
}
