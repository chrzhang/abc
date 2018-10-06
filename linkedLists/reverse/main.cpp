#include <iostream>
#include <cassert>

using namespace std;
// Reverse a linked list in k-groups

struct ListNode {
    int val;
    ListNode * next;
    ListNode(const int v) : val(v), next(nullptr) {}
};

ListNode * raux(ListNode * h, ListNode * e) {
    if (h == e) {
        h->next = nullptr;
        return h;
    }
    ListNode * f = raux(h->next, e);
    f->next = h;
    return h;
}

ListNode * reverse(ListNode * h, ListNode * e) {
    ListNode * r = raux(h, e);
    r->next = nullptr;
    return e;
}

void revk(ListNode * t, const int k) {
    assert(t);
    ListNode * h = t->next;
    ListNode * e = h;
    for (int i = 0; i < k - 1; ++i) {
        if (!e) { return; }
        e = e->next;
    }
    if (!e) { return; }
    t->next = e;
    ListNode * future_h = e->next;
    reverse(h, e);
    h->next = future_h;
    revk(h, k);
}

ListNode* reverseKGroup(ListNode* head, int k) {
    ListNode sentinel(0);
    sentinel.next = head;
    revk(&sentinel, k);
    return sentinel.next;
}

void print_list(const ListNode * head) {
    const ListNode * curr = head;
    while (curr) {
        cout << curr->val << " ";
        curr = curr->next;
    }
    cout << endl;
}

int main() {
    {
        ListNode l1(1);
        ListNode l2(2);
        ListNode l3(3);
        ListNode l4(4);
        ListNode l5(5);
        l1.next = &l2;
        l2.next = &l3;
        l3.next = &l4;
        l4.next = &l5;
        ListNode * result = reverseKGroup(&l1, 2);
        print_list(result);
    }
    {
        ListNode l1(1);
        ListNode l2(2);
        ListNode l3(3);
        ListNode l4(4);
        ListNode l5(5);
        l1.next = &l2;
        l2.next = &l3;
        l3.next = &l4;
        l4.next = &l5;
        ListNode * result = reverseKGroup(&l1, 3);
        print_list(result);
    }
    return 0;
}
