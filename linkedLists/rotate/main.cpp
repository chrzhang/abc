#include <iostream>

struct ListNode {
    int val;
    ListNode * next;
    ListNode(int v) : val(v), next(nullptr) {}
};

struct List {
    ListNode * head;
    size_t size;
    List() : head(nullptr), size(0) {}
    ~List() {
        auto curr = head;
        while (curr) {
            auto temp = curr->next;
            delete curr;
            curr = temp;
        }
    }
    void add(int val) {
        ListNode * n = new ListNode(val);
        auto oldHead = head;
        head = n;
        n->next = oldHead;
        ++size;
    }
    void rotateOnce() {
        if (!head->next) { return; }
        auto tail = head;
        while (tail->next->next) {
            tail = tail->next;
        }
        auto result = tail->next;
        result->next = head;
        tail->next = nullptr;
        head = result;
    }
    void rotate(int k) {
        if (k <= 0 || !head || size <= 1) { return; }
        auto amt = k % size;
        for (int i = 0; i < amt; ++i) {
            rotateOnce();
        }
    }
};

std::ostream & operator<<(std::ostream & os, const List & l) {
    auto curr = l.head;
    while (curr) {
        os << curr->val << " ";
        curr = curr->next;
    }
    os << "\n";
    return os;
}

int main() {
    List l;
    for (int i = 5; i >= 0; --i) {
        l.add(i);
    }
    std::cout << l << std::endl;
    for (int i = 0; i < 10; ++i) {
        std::cout << "Rotating " << i << " times: ";
        l.rotate(i);
        std::cout << l << std::endl;
    }
    return 0;
}
