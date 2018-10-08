class Solution {
public:
    ListNode* oddEvenList(ListNode* head) {
        if (!head) { return nullptr; }
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
        return oddListHead;
    }
};
