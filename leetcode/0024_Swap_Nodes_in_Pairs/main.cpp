class Solution {
public:
    void swaux(ListNode** t, ListNode* c)
    {
        if (c == nullptr) {
            if (*t) {
                (*t)->next = c;
            }
            return;
        }
        if (c->next) {
            if (*t == nullptr) {
                *t = c->next;
            } else {
                (*t)->next = c->next;
            }
            ListNode* n = c->next->next;
            c->next->next = c;
            swaux(&c, n);
        } else {
            if (*t == nullptr) {
                *t = c;
            } else {
                (*t)->next = c;
            }
        }
    }

    ListNode* swapPairs(ListNode* l)
    {
        ListNode* result = nullptr;
        swaux(&result, l);
        return result;
    }
};