class Solution {
public:
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
};