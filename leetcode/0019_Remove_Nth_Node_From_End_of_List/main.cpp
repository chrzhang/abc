class Solution {
public:
    ListNode* removeNthFromEnd(ListNode* head, int n) {

        ListNode * orig_head = head;
        ListNode * advance = head;
        for (int i = 0; i < n; ++i) {
            advance = advance->next;
        }
        for (;;) {
            if (advance == NULL || advance->next == NULL) {
                break;
            }
            advance = advance->next;
            head = head->next;
        }
        if (advance == NULL) {
            return head->next;
        } else {
            head->next = head->next->next;
        }
        return orig_head;
    }
};
