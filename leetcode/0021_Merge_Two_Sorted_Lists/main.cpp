class Solution {
public:
    void add(ListNode** h, ListNode** t, ListNode* n)
    {
        if (*t == NULL) {
            *h = *t = n;
        } else {
            (*t)->next = n;
            *t = n;
        }
    }
    ListNode* mergeTwoLists(ListNode* l1, ListNode* l2)
    {
        if (l1 == NULL)
            return l2;
        if (l2 == NULL)
            return l1;
        ListNode* rh = NULL;
        ListNode* rt = NULL;
        while (l1 != NULL && l2 != NULL) {
            if (l1->val <= l2->val) {
                ListNode* t = l1->next;
                l1->next = NULL;
                add(&rh, &rt, l1);
                l1 = t;
            } else {
                ListNode* t = l2->next;
                l2->next = NULL;
                add(&rh, &rt, l2);
                l2 = t;
            }
        }
        if (l1 != NULL) {
            rt->next = l1;
        } else if (l2 != NULL) {
            rt->next = l2;
        }
        return rh;
    }
};