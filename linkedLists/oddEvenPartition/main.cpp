#include <iostream>

// Given a linked list, modify the list so that all the even nodes (2nd, 4th...)
// appear after the odd ones (1st, 3rd...)

struct ListNode {
    int val;
    ListNode * next;
    ListNode(int v) : val(v), next(nullptr) {};
};

struct List {
    ListNode * head, * tail;
    List() : head(nullptr), tail(nullptr) {}
    ~List() {
        auto curr = head;
        while (curr) {
            auto temp = curr->next;
            delete curr;
            curr = temp;
        }
    }
    void add(ListNode * n) {
        if (!n) { return; }
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
    void partitionOddEvenList() {
        if (!head) { return; }
        ListNode * oddListHead, * oddListTail;
        oddListHead = oddListTail = nullptr;
        ListNode * evenListHead, * evenListTail;
        evenListHead = evenListTail = nullptr;
        ListNode * current = head;
        int index = 1;
        while (current) {
            ListNode * temp = current->next;
            if (index % 2) { // Odd
                if (!oddListHead) {
                    oddListHead = oddListTail = current;
                } else {
                    ListNode * oldTail = oddListTail;
                    oddListTail = current;
                    oldTail->next = oddListTail;

                }
            } else { // Even
                if (!evenListHead) {
                    evenListHead = evenListTail = current;
                } else {
                    ListNode * oldTail = evenListTail;
                    evenListTail = current;
                    oldTail->next = evenListTail;

                }
            }
            current = temp;
            ++index;
        }
        oddListTail->next = evenListHead;
        if (evenListTail) { evenListTail->next = nullptr; }
        head = oddListHead;
        tail = evenListTail;

    }
};

int main() {
    List l;
    for (int i = 0; i < 10; ++i) {
        l.add(new ListNode(i));
    }
    l.print();
    l.partitionOddEvenList();
    l.print();
    return 0;
}
