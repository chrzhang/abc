#include <iostream>
#include <cstdlib>
#include <ctime>

#define N 60

// Merge two sorted linked lists

struct Node {
    int val;
    Node * next;
    Node(int v) : val(v), next(nullptr) {}
};

struct LinkedList {
    Node * head;
    Node * tail;
    LinkedList() {
        head = nullptr;
    }
    ~LinkedList() {
        Node * current = head;
        while (current) {
            Node * temp = current->next;
            delete current;
            current = temp;
        }
    }
    void print() const {
        Node * current = head;
        while (current) {
            std::cout << current->val << " ";
            current = current->next;
        }
        std::cout << std::endl;
    }
    void add(Node * n) {
        if (!n) { return; }
        if (!head) {
            head = tail = n;
        } else {
            Node * oldTail = tail;
            tail = n;
            oldTail->next = tail;
        }
    }
    void add(int val) {
        Node * n = new Node(val);
        add(n);
    }
};

LinkedList merge(const LinkedList & l1, const LinkedList & l2) {
    LinkedList finalList;
    Node * n1 = l1.head;
    Node * n2 = l2.head;
    while (n1 || n2) {
        if (n1 && n2) {
            if (n1->val < n2->val) {
                finalList.add(n1->val);
                n1 = n1->next;
            } else {
                finalList.add(n2->val);
                n2 = n2->next;
            }
        } else if (n1) {
            finalList.add(n1->val);
            n1 = n1->next;
        } else if (n2) {
            finalList.add(n2->val);
            n2 = n2->next;
        }
    }
    return finalList;
}

int main() {
    srand(time(0));
    LinkedList l1;
    for (int i = 0; i < N; i += rand() % 10) {
        l1.add(i);
    }
    std::cout << "List 1: ";
    l1.print();
    LinkedList l2;
    for (int i = 0; i < N; i += rand() % 10) {
        l2.add(i);
    }
    std::cout << "List 2: ";
    l2.print();
    LinkedList merged = merge(l1, l2);
    std::cout << "Merged: ";
    merged.print();
    return 0;
}
